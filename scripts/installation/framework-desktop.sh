#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git bitwarden-cli jq restic rsync

set -euo pipefail

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

prompt_confirm() {
  while true; do
    read -r -p "${1:-Continue?} [y/n]: " reply
    case $reply in
    [yY][eE][sS] | [yY])
      return 0
      ;;
    [nN][oO] | [nN])
      return 1
      ;;
    *) printf " \033[31m %s \n\033[0m" "invalid input" ;;
    esac
  done
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
restic snapshots --compact
restic restore latest --target /mnt/persist

colorPrint "🔍 Verifying restore..."
ls /mnt/persist/var || true

# ─────────────────────────────────────────
# Restore /games and /home/media from external device
# ─────────────────────────────────────────
colorPrint ""
colorPrint "Do you want to restore /games and /home/media from an external device?"
if prompt_confirm; then
    lsblk
    colorPrint ""

    while true; do
        colorPrint "📀 Choose the source device (e.g., sda, sdb, nvme1n1):"
        read -r media_disk

        # Check if device exists
        if [ ! -b "/dev/${media_disk}" ]; then
            errorPrint "❌ Device /dev/${media_disk} does not exist"
            lsblk
            continue
        fi

        # Check if it's a disk (not a partition)
        if ! lsblk -ndo TYPE "/dev/${media_disk}" | grep -q "disk"; then
            errorPrint "❌ /dev/${media_disk} is not a disk device"
            lsblk
            continue
        fi

        break
    done

    colorPrint ""
    colorPrint "You selected: /dev/${media_disk}"
    lsblk "/dev/${media_disk}"
    colorPrint ""

    colorPrint "Available partitions:"
    lsblk -o NAME,SIZE,FSTYPE,LABEL,MOUNTPOINT "/dev/${media_disk}"
    colorPrint ""

    colorPrint "📂 Enter the partition number to mount (e.g., 1 for ${media_disk}1):"
    read -r partition_num

    media_partition="/dev/${media_disk}${partition_num}"

    if [ ! -b "$media_partition" ]; then
        errorPrint "❌ Partition $media_partition does not exist. Skipping media restore."
    else
        colorPrint "📦 Mounting $media_partition..."
        mkdir -p /mnt/media_source

        if mount "$media_partition" /mnt/media_source; then
            colorPrint "✅ Mounted $media_partition at /mnt/media_source"
            colorPrint ""
            colorPrint "Contents:"
            ls -lh /mnt/media_source
            colorPrint ""

            # ─────────────────────────────────────────
            # Restore /games
            # ─────────────────────────────────────────
            colorPrint "Enter the path to the /games backup directory (relative to mount point):"
            colorPrint "Example: if data is in /mnt/media_source/backups/games, enter: backups/games"
            colorPrint "Leave empty to skip /games restore."
            read -r games_path

            if [ -n "$games_path" ]; then
                source_path="/mnt/media_source/${games_path}"

                if [ ! -d "$source_path" ]; then
                    errorPrint "❌ Directory $source_path not found. Skipping /games restore."
                else
                    # Mount /games subvolume
                    colorPrint "📦 Mounting /games subvolume..."
                    mkdir -p /mnt/games
                    mount -o subvol=games /dev/mapper/cryptroot /mnt/games

                    colorPrint "🚀 Copying /games data from $source_path..."
                    colorPrint "This may take a while depending on data size..."

                    rsync -avh --info=progress2 "$source_path/" /mnt/games/

                    colorPrint "✅ /games restore complete"

                    # Cleanup
                    umount /mnt/games
                    rmdir /mnt/games
                fi
            else
                colorPrint "⏭️  Skipping /games restore."
            fi

            # ─────────────────────────────────────────
            # Restore /home/media
            # ─────────────────────────────────────────
            colorPrint ""
            colorPrint "Enter the path to the /home/media backup directory (relative to mount point):"
            colorPrint "Example: if data is in /mnt/media_source/backups/home_media, enter: backups/home_media"
            colorPrint "Leave empty to skip /home/media restore."
            read -r home_media_path

            if [ -n "$home_media_path" ]; then
                home_source_path="/mnt/media_source/${home_media_path}"

                if [ ! -d "$home_source_path" ]; then
                    errorPrint "❌ Directory $home_source_path not found. Skipping /home/media restore."
                else
                    # Mount /home/media subvolume
                    colorPrint "📦 Mounting /home/media subvolume..."
                    mkdir -p /mnt/home_media_target
                    mount -o subvol=media /dev/mapper/cryptroot /mnt/home_media_target

                    colorPrint "🚀 Copying /home/media data from $home_source_path..."
                    colorPrint "This may take a while depending on data size..."

                    rsync -avh --info=progress2 "$home_source_path/" /mnt/home_media_target/

                    colorPrint "✅ /home/media restore complete"

                    # Fix permissions for media user (UID 800, GID 1800)
                    colorPrint "🔧 Setting ownership to media user (800:1800)..."
                    chown -R 800:1800 /mnt/home_media_target

                    # Cleanup
                    umount /mnt/home_media_target
                    rmdir /mnt/home_media_target
                fi
            else
                colorPrint "⏭️  Skipping /home/media restore."
            fi

            # Cleanup external device mount
            umount /mnt/media_source
            rmdir /mnt/media_source
        else
            errorPrint "❌ Failed to mount $media_partition. Skipping media restore."
        fi
    fi
else
    colorPrint "⏭️  Skipping media restore. Subvolumes will start empty."
fi

# ─────────────────────────────────────────
# Cleanup
# ─────────────────────────────────────────
colorPrint ""
colorPrint "🧹 Cleaning up mounts..."
umount -R /mnt/persist || errorPrint "Failed to unmount /mnt recursively. Check manually."

colorPrint ""
colorPrint "🎉 Restore complete. System is ready for first boot."
colorPrint "You can now reboot."
