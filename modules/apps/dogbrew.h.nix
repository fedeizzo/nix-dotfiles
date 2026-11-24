{
  # dogbrew self-distributes into /opt/dogbrew (the Homebrew cask is gone).
  # `dogbrew self setup` cannot edit the read-only Home Manager zshrc, so its
  # canonical hook is declared here instead (kept verbatim so `dogbrew doctor`
  # recognises it). The hook is static: it prepends /opt/dogbrew/shims/bin and
  # /opt/dogbrew/bin to PATH and adds dogbrew's zsh completions to fpath.
  flake.modules.homeManager.dogbrew = { lib, ... }: let
    hook = ''
      [ -f "/opt/dogbrew/share/etc/dogbrew/init.zsh" ] && . "/opt/dogbrew/share/etc/dogbrew/init.zsh"
    '';
  in {
    # Login shells (e.g. `zsh -l -c` from GUI apps), which `dogbrew doctor` checks too.
    programs.zsh.profileExtra = hook;

    # Run after everything else (profile-work's mkAfter block sources
    # ~/.dd-zshrc, which re-prepends /opt/homebrew/bin), so the dogbrew
    # shims and binaries win over Homebrew's.
    programs.zsh.initContent = lib.mkOrder 1600 hook;
  };
}
