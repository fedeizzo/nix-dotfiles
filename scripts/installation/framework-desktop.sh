#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git bitwarden-cli jq restic

set -euo pipefail

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

# ─────────────────────────────────────────
# Validate secret is not empty
# ─────────────────────────────────────────
require_not_empty() {
  local value="$1"
  local name="$2"

  if [ -z "$value" ]; then
    errorPrint "❌ Secret '$name' is empty. Aborting."
    exit 1
  fi
}

if [ "$(id -u)" -ne 0 ]; then
  errorPrint "Please run as root (sudo su)"
  exit 1
fi

ulimit -n 65535

# ─────────────────────────────────────────
# Bitwarden login/unlock (password asked once)
# ─────────────────────────────────────────
colorPrint "🔐 Unlocking Bitwarden vault..."

read -rp "🔐 Enter Bitwarden email: " BW_EMAIL
read -rsp "🔐 Enter Bitwarden account password: " BW_ACC_PASSWORD
echo
read -rsp "🔐 Enter Bitwarden master password: " BW_PASSWORD
echo

# Check login status
if ! bw login --check &>/dev/null; then
  colorPrint "Logging into Bitwarden..."
  BW_SESSION=$(bw login $BW_EMAIL $BW_ACC_PASSWORD --raw)
  export BW_SESSION
fi

# Unlock and capture session
BW_SESSION="$(bw unlock --raw "$BW_PASSWORD")"
export BW_SESSION

# Clear password variable immediately
unset BW_PASSWORD

# Sync vault to avoid stale data
bw sync

# ─────────────────────────────────────────
# Fetch SOPS key
# ─────────────────────────────────────────
SOPS_KEY_FILE="/var/lib/sops/keys.txt"
mkdir -p /var/lib/sops

if [ ! -f "$SOPS_KEY_FILE" ]; then
  colorPrint "🔑 Fetching SOPS keys from Bitwarden..."

  secret="$(bw get item 'sops-age-keys-homelab' | jq -r .notes)"
  require_not_empty "$secret" "sops-age-keys-homelab"

  echo "$secret" > "$SOPS_KEY_FILE"
  chmod 600 "$SOPS_KEY_FILE"
fi

# ─────────────────────────────────────────
# Fetch Restic B2 credentials
# ─────────────────────────────────────────
CRED_FILE="/var/lib/.restic_b2_env"
RESTIC_PASS_FILE="/var/lib/.restic_b2_password"

if [ -f "$CRED_FILE" ]; then
    colorPrint "🔑 Loading Restic/B2 credentials from $CRED_FILE..."
    # shellcheck disable=SC1090
    source "$CRED_FILE"
else
    colorPrint "🗄️ Fetching Restic/B2 credentials from Bitwarden..."

    BACKBLAZE_ITEM="$(bw get item 'backblaze')"

    RESTIC_REPOSITORY="b2:$(echo "$BACKBLAZE_ITEM" | jq -r '.fields[] | select(.name=="B2_BUCKET") | .value')"
    B2_ACCOUNT_ID="$(echo "$BACKBLAZE_ITEM" | jq -r '.fields[] | select(.name=="B2_ACCOUNT_ID") | .value')"
    B2_ACCOUNT_KEY="$(echo "$BACKBLAZE_ITEM" | jq -r '.fields[] | select(.name=="B2_ACCOUNT_KEY") | .value')"

    require_not_empty "$RESTIC_REPOSITORY" "B2_BUCKET"
    require_not_empty "$B2_ACCOUNT_ID" "B2_ACCOUNT_ID"
    require_not_empty "$B2_ACCOUNT_KEY" "B2_ACCOUNT_KEY"

    RESTIC_PASSWORD="$(bw get item 'resticBackup' | jq -r '.login.password')"
    require_not_empty "$RESTIC_PASSWORD" "resticBackup password"

    echo "$RESTIC_PASSWORD" > "$RESTIC_PASS_FILE"
    chmod 600 "$RESTIC_PASS_FILE"

    cat > "$CRED_FILE" <<EOF
export RESTIC_REPOSITORY="$RESTIC_REPOSITORY"
export B2_ACCOUNT_ID="$B2_ACCOUNT_ID"
export B2_ACCOUNT_KEY="$B2_ACCOUNT_KEY"
export RESTIC_PASSWORD_FILE="$RESTIC_PASS_FILE"
EOF

    chmod 600 "$CRED_FILE"

    colorPrint "💾 Saved credentials to $CRED_FILE"
fi

colorPrint "📦 B2 repository: $RESTIC_REPOSITORY"

colorPrint "🧠 Increasing tmpfs from 64G to 110G"
mount -o remount,size=110G /nix/.rw-store

# ─────────────────────────────────────────
# Run Disko install
# ─────────────────────────────────────────
colorPrint "💾 Running disko-install..."
nix --extra-experimental-features 'nix-command flakes' \
  run github:nix-community/disko#disko-install -- \
  --flake '.#homelab' \
  --disk main /dev/nvme0n1 \
  --extra-files "$SOPS_KEY_FILE" "$SOPS_KEY_FILE"

colorPrint "✅ Disko complete."

# ─────────────────────────────────────────
# Verify /mnt/persist
# ─────────────────────────────────────────
if mountpoint -q /mnt/persist; then
    colorPrint "/mnt/persist is already mounted."
else
    colorPrint "/mnt/persist not mounted, mounting manually..."
    mkdir -p /mnt/persist
    mount -o subvol=persist /dev/mapper/cryptroot /mnt/persist
fi

# ─────────────────────────────────────────
# Restore backup
# ─────────────────────────────────────────
colorPrint "🚀 Restoring latest snapshot into /mnt/persist ..."
restic snapshots
restic restore latest --target /mnt/persist

colorPrint "🔍 Verifying restore..."
ls /mnt/persist/var || true

# ─────────────────────────────────────────
# Cleanup
# ─────────────────────────────────────────
colorPrint "🧹 Cleaning up mounts..."
umount -R /mnt/persist || errorPrint "Failed to unmount /mnt recursively. Check manually."

colorPrint "🎉 Restore complete. System is ready for first boot."
colorPrint "You can now reboot."
