# jj-sign-ssh: ssh-keygen shim for jj's ssh signing backend.
#
# jj calls this like ssh-keygen (`-Y sign -f <key> -n git`, `-Y verify -f
# <allowed-signers> ...`). The `-f` value jj passes is a placeholder; it is
# replaced by what git itself would use for the repository jj operates on:
#   -Y sign:      user.signingKey (gpg.format must be ssh)
#   other modes:  gpg.ssh.allowedSignersFile
# git is asked with --git-dir pointing at the repository's git store, so any
# per-repository logic in the user's git config (e.g. includeIf
# hasconfig:remote.*.url) applies exactly as it does for git commits.
#
# Escape hatch: JJ_SIGN_SSH_KEY=<public key | path> forces the signing key.
# Debugging:    JJ_SIGN_SSH_DEBUG=1 explains the decision on stderr.

die() {
  echo "jj-sign-ssh: error: $*" >&2
  exit 1
}

debug() {
  if [[ -n ${JJ_SIGN_SSH_DEBUG:-} ]]; then echo "jj-sign-ssh: $*" >&2; fi
}

expand_home() {
  case $1 in
    \~) echo "$HOME" ;;
    \~/*) echo "$HOME/${1:2}" ;;
    *) echo "$1" ;;
  esac
}

mode=""
prev=""
for arg in "$@"; do
  if [[ $prev == -Y ]]; then
    mode=$arg
    break
  fi
  prev=$arg
done

# --- locate the repository ---------------------------------------------------
# jj runs us with its own cwd, which is not the repository for
# `jj -R <path>`. Peek at the parent jj's argv for -R/--repository. ps loses
# quoting, so candidates only count if they are a jj workspace; if -R was seen
# but none resolves, refuse to guess.
saw_repo_flag=""
repo_arg=""
parent_args=()
parent_comm=$(ps -o comm= -p "$PPID" 2>/dev/null || true)
case ${parent_comm##*/} in
  jj | .jj-wrapped)
    read -r -a parent_args < <(ps -ww -o args= -p "$PPID" 2>/dev/null || true) || true
    ;;
esac
repo_candidates=()
for ((i = 1; i < ${#parent_args[@]}; i++)); do
  arg=${parent_args[i]}
  case $arg in
    --) break ;;
    -R | --repository) repo_candidates+=("${parent_args[i + 1]:-}") ;;
    --repository=*) repo_candidates+=("${arg#--repository=}") ;;
    -R?*) repo_candidates+=("${arg#-R}") ;;
  esac
done
if ((${#repo_candidates[@]} > 0)); then
  saw_repo_flag=1
  for cand in "${repo_candidates[@]}"; do
    if [[ -n $cand && -d $cand/.jj ]]; then
      repo_arg=$cand
      break
    fi
  done
fi

find_workspace_root() {
  local dir
  dir=$(cd "$1" 2>/dev/null && pwd -P) || return 1
  while :; do
    if [[ -d $dir/.jj ]]; then
      echo "$dir"
      return 0
    fi
    [[ $dir == / ]] && return 1
    dir=$(dirname "$dir")
  done
}

# Prints the git dir backing a jj workspace (colocated or not, main or
# secondary workspace). Prints nothing if there is no git store.
git_dir_for_workspace() {
  local root=$1 repo store target
  repo="$root/.jj/repo"
  if [[ -f $repo ]]; then # secondary workspace: file pointing at the repo
    target=$(<"$repo")
    [[ $target == /* ]] || target="$root/.jj/$target"
    repo=$target
  fi
  store="$repo/store"
  if [[ -f $store/git_target ]]; then
    target=$(<"$store/git_target")
    [[ $target == /* ]] || target="$store/$target"
    echo "$target"
  elif [[ -d $root/.git ]]; then
    echo "$root/.git"
  fi
}

# Sets git_dir, or explains in why_no_repo why it could not.
git_dir=""
why_no_repo=""
if [[ -n $saw_repo_flag && -z $repo_arg ]]; then
  why_no_repo="jj was given -R/--repository (${repo_candidates[*]}) but none of the values is a jj workspace"
else
  start_dir=${repo_arg:-$PWD}
  if root=$(find_workspace_root "$start_dir"); then
    git_dir=$(git_dir_for_workspace "$root")
    [[ -n $git_dir ]] || why_no_repo="jj workspace $root has no git store"
    debug "workspace $root, git dir ${git_dir:-<none>}"
  else
    why_no_repo="no jj workspace found at or above '$start_dir'"
  fi
fi

# git config as seen from the repository; never from an unrelated cwd.
repo_git_config() {
  git --git-dir="$git_dir" config "$@" || true
}

# Rewrites "$@" with the value of -f replaced by $1.
replace_f() {
  local new_f=$1 skip=""
  shift
  out_args=()
  for arg in "$@"; do
    if [[ -n $skip ]]; then
      skip=""
      continue
    fi
    if [[ $arg == -f ]]; then
      skip=1
      out_args+=(-f "$new_f")
      continue
    fi
    out_args+=("$arg")
  done
}

# --- verify, find-principals, check-novalidate, ... ------------------------
if [[ $mode != sign ]]; then
  if [[ -n $git_dir ]]; then
    signers=$(repo_git_config --type=path --get gpg.ssh.allowedSignersFile)
  else
    debug "$why_no_repo; using allowed signers from the user/system git config"
    signers=$(cd / && git config --type=path --get gpg.ssh.allowedSignersFile || true)
  fi
  if [[ -z $signers ]]; then
    exec ssh-keygen "$@"
  fi
  replace_f "$signers" "$@"
  exec ssh-keygen "${out_args[@]}"
fi

# --- sign --------------------------------------------------------------------
if [[ -n ${JJ_SIGN_SSH_KEY:-} ]]; then
  key=$JJ_SIGN_SSH_KEY
  key_src="JJ_SIGN_SSH_KEY"
else
  [[ -n $git_dir ]] ||
    die "$why_no_repo; cannot tell which signing key git would use. Run jj from inside the workspace or set JJ_SIGN_SSH_KEY."
  format=$(repo_git_config --get gpg.format)
  [[ $format == ssh ]] ||
    die "git uses gpg.format='${format:-openpgp}' for $git_dir, not ssh"
  key=$(repo_git_config --get user.signingKey)
  [[ -n $key ]] || die "git has no user.signingKey for $git_dir"
  key_src="git config for $git_dir"
fi

key=${key#key::}
extra_args=()
case $key in
  ssh-* | sk-* | ecdsa-*)
    key_file=$(mktemp "${TMPDIR:-/tmp}/jj-sign-ssh.XXXXXX")
    trap 'rm -f "$key_file"' EXIT
    printf '%s\n' "$key" >"$key_file"
    extra_args=(-U) # literal public key: the private half lives in the agent
    ;;
  *)
    key_file=$(expand_home "$key")
    [[ -r $key_file ]] || die "signing key file '$key_file' (from $key_src) is not readable"
    ;;
esac
debug "signing with $(ssh-keygen -lf "$key_file" 2>/dev/null || echo "$key_file") from $key_src"

replace_f "$key_file" "$@"
ssh-keygen "${extra_args[@]}" "${out_args[@]}"
