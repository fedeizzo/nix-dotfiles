#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git bitwarden-cli jq restic

set -euo pipefail

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

if [ "$(id -u)" -ne 0 ]; then
  errorPrint "Please run as root (sudo su)"
  exit 1
fi

# avoid to many files open in the nix store
ulimit -n 65535

# ─────────────────────────────────────────
# Bitwarden login
# ─────────────────────────────────────────
colorPrint "🔐 Checking Bitwarden login..."
if ! bw login --check &>/dev/null; then
  bw login
fi

# ─────────────────────────────────────────
# Fetch SOPS key
# ─────────────────────────────────────────
SOPS_KEY_FILE="/var/lib/sops/keys.txt"
mkdir -p /var/lib/sops
if [ ! -f "$SOPS_KEY_FILE" ]; then
  colorPrint "🔑 Fetching SOPS keys from Bitwarden..."
  while true; do
    secret=$(bw get item 'sops-age-keys-homelab' | jq -r .notes)
    if [ $? -eq 0 ]; then
      echo "$secret" > "$SOPS_KEY_FILE"
      chmod 600 "$SOPS_KEY_FILE"
      break
    else
      errorPrint "Failed to fetch SOPS keys, retrying..."
      sleep 1
    fi
  done
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
    RESTIC_REPOSITORY="b2:$(bw get item 'backblaze' | jq -r '.fields[] | select(.name=="B2_BUCKET") | .value')"
    B2_ACCOUNT_ID="$(bw get item 'backblaze' | jq -r '.fields[] | select(.name=="B2_ACCOUNT_ID") | .value')"
    B2_ACCOUNT_KEY="$(bw get item 'backblaze' | jq -r '.fields[] | select(.name=="B2_ACCOUNT_KEY") | .value')"

    # Save the Restic password to a secure file
    bw get item 'resticBackup' | jq -r '.login.password' > "$RESTIC_PASS_FILE"
    chmod 600 "$RESTIC_PASS_FILE"

    # Save the env file pointing to the password file
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
# Cleanup: Unmount Disko mounts
# ─────────────────────────────────────────
colorPrint "🧹 Cleaning up mounts..."
umount -R /mnt || errorPrint "Failed to unmount /mnt recursively. Check manually."

colorPrint "🎉 Restore complete. System is ready for first boot."
colorPrint "You can now reboot."
