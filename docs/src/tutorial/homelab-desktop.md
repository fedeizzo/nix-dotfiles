# Setting Up Homelab Desktop (x86_64 Linux)

This guide walks through setting up a new Linux desktop with NixOS using the homelab configuration. This setup uses BTRFS with LUKS encryption, impermanence for persistence, SOPS for secrets management, and Restic for backups.

## Prerequisites

### Hardware Requirements
- **Disk**: NVMe SSD (minimum 256GB recommended)
- **RAM**: 8GB minimum, 16GB+ recommended
- **Network**: Ethernet or Wi-Fi connectivity

### Software Requirements
- **NixOS Installer ISO**: Download from [nixos.org](https://nixos.org/download.html)
- **Bitwarden Account**: For secrets management
- **Backblaze B2 Account**: For backups (optional - can be configured later)
- **External Storage**: For migrating existing data (optional)

### Tools Needed (pre-installed in NixOS installer)
The NixOS installer includes most tools needed. Additional tools are installed by the setup script:
- `git`
- `bitwarden-cli` (bw)
- `jq`
- `restic`
- `rsync`

## Overview of Setup

This installation creates:
- **Encrypted BTRFS filesystem** with subvolumes for root, Nix, logs, SOPS, persistence, games, and media
- **LUKS2 encryption** on the entire disk (except ESP)
- **Weekly root subvolume rotation** (keeps 7 days of snapshots)
- **Persistent data** across reboots via `/persist` subvolume
- **Automatic backup** integration with Restic and Backblaze B2

### Filesystem Layout

```
/dev/nvme0n1
├── ESP (512MB, VFAT)          → /boot
└── LUKS encrypted
    └── BTRFS
        ├── /root              → / (root filesystem)
        ├── /nix               → /nix (Nix store)
        ├── /var/log           → /var/log (system logs)
        ├── /var/lib/sops      → /var/lib/sops (SOPS cache)
        ├── /persist           → /persist (persistent data)
        ├── /games             → /games (gaming data)
        └── /home/media        → /home/media (media files)
```

## Step-by-Step Installation

### 1. Boot the NixOS Installer

1. Insert the NixOS installer USB and boot from it
2. Select "Boot NixOS" from the menu
3. Once booted, switch to a root shell:
   ```bash
   sudo su
   ```

### 2. Prepare the System

Increase the tmpfs size for the build process:
```bash
mount -o remount,size=110G /nix/.rw-store
```

**Why?** The Nix build process requires significant temporary space. The default 64GB is often insufficient for building large packages.

### 3. Clone the Dotfiles Repository

```bash
cd /root
git clone https://github.com/your-org/nix-dotfiles.git
cd nix-dotfiles
```

### 4. Run the Installation Script

The installation script automates the entire setup process:

```bash
./scripts/installation/framework-desktop.sh
```

The script will:

#### 4.1 Bitwarden Integration
- Prompt for Bitwarden email and master password
- Unlock your Bitwarden vault
- Fetch SOPS age keys from Bitwarden item `sops-age-keys-homelab`
- Fetch Restic/B2 credentials from Bitwarden items `backblaze` and `resticBackup`

**Security Note**: Credentials are cached locally in `/var/lib/sops/` and `/var/lib/` after first setup. They are stored with restrictive permissions (600).

#### 4.2 Disk Setup with Disko
The script runs Disko to partition and format the disk:

```bash
nix run github:nix-community/disko#disko-install \
  --flake '.#homelab' \
  --disk main /dev/nvme0n1 \
  --extra-files "$SOPS_KEY_FILE" "$SOPS_KEY_FILE"
```

**Important**: This will **erase all data** on `/dev/nvme0n1`. Ensure you have the correct disk selected.

The disko configuration creates:
- 512MB EFI System Partition (ESP)
- LUKS2 encrypted BTRFS volume with multiple subvolumes
- Weekly cleanup of old root subvolumes (keeps 7 days)

#### 4.3 Backup Restoration
After disk setup, the script:
1. Restores the latest Restic snapshot to `/mnt/persist`
2. Optionally restores `/games` and `/home/media` from external storage

**To restore from external storage:**
1. Connect your external drive
2. When prompted, select the device (e.g., `sda`, `sdb`)
3. Select the partition containing your backup data
4. Enter the relative path to your `/games` and `/home/media` backups

### 5. Verify the Installation

Check that the filesystem is mounted correctly:
```bash
mount | grep -E "(persist|sops|log)"
```

You should see:
- `/persist` mounted
- `/var/log` mounted
- `/var/lib/sops` mounted

### 6. Reboot

```bash
reboot
```

Remove the installer USB when prompted.

## Post-Installation Configuration

### First Boot

1. **Enter LUKS passphrase** at boot
2. The system will boot into the restored state from your last backup

### Configure New User (if needed)

If you're setting up a fresh system without restoration:

1. Edit `/home/oven/nix-dotfiles/nix/nixosConfigurations.nix`
2. Update the `homelab` configuration with your username and SSH keys
3. Rebuild and deploy:
   ```bash
   sudo nixos-rebuild switch --flake .#homelab
   ```

### Set Up SSH Access

Add your SSH public key to the configuration:

```nix
# In hosts/framework-desktop/users.nix or similar
users.users.youruser = {
  isNormalUser = true;
  extraGroups = [ "wheel" "networkmanager" ];
  openssh.authorizedKeys.keys = [ "ssh-rsa AAAA..." ];
};
```

### Configure Wi-Fi (if applicable)

```nix
# In hosts/framework-desktop/system/wifi.nix
networking.wireless = {
  enable = true;
  userControlled.enable = true;
};

networking.wireless.networks."your-ssid" = {
  psk = "your-password";
};
```

### First Time SOPS Setup

After first boot, SOPS needs to be initialized:

```bash
# The credentials should already be in /var/lib/sops/
# If not, run the setup script again or manually fetch from Bitwarden
```

### Restic Backup Verification

Check that backups are working:
```bash
restic snapshots
```

You should see at least one snapshot from the installation restore.

## Key Configuration Decisions

### Why BTRFS?

BTRFS provides:
- **Subvolumes**: Logical separation of data (root, nix, persist, games, media)
- **Compression**: zstd:3 compression saves space (enabled on all subvolumes)
- **Snapshots**: Easy rollback capability
- **Copy-on-write**: Data integrity

### Why LUKS Encryption?

Full disk encryption protects:
- Sensitive data at rest
- SSH keys and secrets
- Personal files and media

### Why Impermanence?

The `/persist` subvolume strategy:
- Keeps `/home`, `/etc`, and other mutable data across reboots
- Allows clean system updates without losing configuration
- Simplifies backup strategy (only persist needs backup)

### Why Weekly Root Rotation?

Keeping 7 days of root snapshots:
- Protects against bad updates
- Allows rollback to previous working state
- Automatic cleanup prevents disk exhaustion

### Why Separate /games and /home/media?

Large data subvolumes:
- Easier to backup selectively
- Can be restored independently
- Better space management
- Media user ownership (UID 800, GID 1800)

## Troubleshooting

### LUKS Passphrase Issues

If you forget your LUKS passphrase:
- **No recovery possible** - LUKS encryption is designed to be unbreakable without the passphrase
- You'll need to reinstall and restore from backup

### Boot Fails After Update

If the system fails to boot after an update:
1. Boot from NixOS installer USB
2. Mount and chroot:
   ```bash
   mount /dev/mapper/cryptroot /mnt
   mount /dev/nvme0n1p1 /mnt/boot
   chroot /mnt
   nixos-rebuild switch --flake .#homelab
   ```

### SOPS Errors

If SOPS cannot decrypt secrets:
```bash
# Check if keys exist
ls -la /var/lib/sops/keys.txt

# If missing, fetch from Bitwarden manually
bw get item 'sops-age-keys-homelab' | jq -r .notes > /var/lib/sops/keys.txt
chmod 600 /var/lib/sops/keys.txt
```

### Restic Backup Failures

If backups fail:
```bash
# Check credentials
cat /var/lib/.restic_b2_env

# Test connection
restic snapshots

# If credentials expired, re-fetch from Bitwarden
bw get item 'backblaze' | jq -r '.fields[] | select(.name=="B2_BUCKET") | .value'
```

### Disk Space Issues

If disk fills up:
```bash
# Check disk usage
df -h

# Clean old root snapshots (manual intervention)
mount /dev/mapper/cryptroot /mnt
btrfs subvolume list -o /mnt/root
btrfs subvolume delete /mnt/root/old_roots/oldest-snapshot
```

### Media Subvolume Permissions

If media files have wrong ownership:
```bash
# Fix permissions
chown -R 800:1800 /home/media
```

## Maintenance

### Regular Updates

```bash
# System update
sudo nixos-rebuild switch --flake .#homelab

# Home Manager update (if applicable)
home-manager switch --flake .#yourusername@homelab
```

### Manual Backup

Create a manual backup:
```bash
restic backup /persist
```

### Check Backup Status

```bash
restic snapshots
restic stats
```

### Monitor Disk Health

```bash
# SMART data
smartctl -a /dev/nvme0n1

# BTRFS health
btrfs filesystem usage /
btrfs scrub start /
```

## Next Steps

1. **Customize your configuration**: Edit the flake to add packages and services
2. **Set up additional backups**: Consider local backups in addition to cloud
3. **Configure networking**: Set up static IPs, DNS, firewalls as needed
4. **Install desktop environment**: Add GNOME, KDE, or your preferred DE
5. **Set up gaming**: Configure Steam, Lutris, etc. in `/games`

## References

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Disko Documentation](https://github.com/nix-community/disko)
- [BTRFS Documentation](https://btrfs.wiki.kernel.org/index.php/Main_Page)
- [SOPS Documentation](https://github.com/getsops/sops)
- [Restic Documentation](https://restic.readthedocs.io/)