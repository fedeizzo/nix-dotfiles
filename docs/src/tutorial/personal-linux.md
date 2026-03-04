# Setting Up Personal Linux (ThinkPad X1 Nano)

This guide walks through setting up a new ThinkPad X1 Nano with NixOS using the personal configuration. This setup uses BTRFS with LUKS encryption, impermanence for persistence, and is optimized for laptop use with features like sleep/wake support and battery management.

## Prerequisites

### Hardware Requirements
- **Laptop**: ThinkPad X1 Nano (Gen 1, 2, or 3)
- **Disk**: NVMe SSD (minimum 256GB, 512GB+ recommended)
- **RAM**: 16GB minimum, 32GB+ recommended
- **Network**: Wi-Fi (Intel or Broadcom) and Ethernet (via USB-C dock)

### Software Requirements
- **NixOS Installer ISO**: Download from [nixos.org](https://nixos.org/download.html)
- **Bitwarden Account**: For secrets management
- **Backblaze B2 Account**: For backups (optional)
- **External USB Drive**: For migration (optional, for swap disk option)

### Tools Needed (pre-installed in NixOS installer)
- `git`
- `bitwarden-cli` (bw)
- `jq`
- `restic`
- `rsync`

## Overview of Setup

This installation creates:
- **Encrypted BTRFS filesystem** with subvolumes for root, Nix, logs, SOPS, and persistence
- **LUKS2 encryption** on the entire disk (except ESP)
- **Weekly root subvolume rotation** (keeps 7 days of snapshots)
- **Persistent data** across reboots via `/persist` subvolume
- **Optional external swap disk** for hibernation support
- **Laptop-optimized settings**: Power management, sleep/wake support

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
        └── [Optional]         → /dev/sdX (external swap disk)
```

### Laptop-Specific Features

This configuration includes:
- **Sleep/Wake support**: Proper suspend/resume handling
- **Power management**: CPU scaling, disk power saving
- **Battery management**: TLP or similar for battery optimization
- **Touchpad configuration**: GNOME touchpad settings
- **Keyboard backlight**: Automatic brightness control
- **Wi-Fi power saving**: Balanced performance and battery life

## Step-by-Step Installation

### 1. Boot the NixOS Installer

1. Insert the NixOS installer USB and boot from it
2. Select "Boot NixOS" from the menu
3. Once booted, switch to a root shell:
   ```bash
   sudo su
   ```

### 2. Test Hardware (Optional but Recommended)

Before installing, test that all hardware works:

```bash
# Test Wi-Fi
nmcli device wifi list

# Test touchpad
libinput list-devices

# Test keyboard backlight
cat /proc/acpi/button/power/state

# Test sleep
systemctl suspend
# Press a key to wake up
```

If any hardware doesn't work, check the NixOS hardware compatibility list and adjust the configuration accordingly.

### 3. Clone the Dotfiles Repository

```bash
cd /root
git clone https://github.com/your-org/nix-dotfiles.git
cd nix-dotfiles
```

### 4. Run the Installation Script

```bash
./scripts/installation/x1-nano.sh
```

The script will:

#### 4.1 Bitwarden Integration
- Prompt for Bitwarden email and master password
- Unlock your Bitwarden vault
- Fetch SOPS age keys from Bitwarden item `sops-age-keys-x1-nano`
- Fetch Restic/B2 credentials from Bitwarden items

#### 4.2 Disk Setup with Disko
The script runs Disko to partition and format the internal NVMe:

```bash
nix run github:nix-community/disko#disko-install \
  --flake '.#x1-nano' \
  --disk main /dev/nvme0n1 \
  --extra-files "$SOPS_KEY_FILE" "$SOPS_KEY_FILE"
```

**Important**: This will **erase all data** on `/dev/nvme0n1`.

The disko configuration creates:
- 512MB EFI System Partition (ESP)
- LUKS2 encrypted BTRFS volume with subvolumes
- Weekly cleanup of old root subvolumes (keeps 7 days)

#### 4.3 Optional External Swap Disk

If you want to use an external USB drive for swap/hibernation:

1. Connect the external USB drive
2. When prompted, select the device (e.g., `sda`)
3. The script will format it as swap space

**Note**: External swap is optional and mainly useful for hibernation support.

#### 4.4 Backup Restoration
After disk setup, the script:
1. Restores the latest Restic snapshot to `/mnt/persist`
2. Optionally restores data from external storage

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
2. Update the `x1-nano` configuration with your username and SSH keys
3. Rebuild and deploy:
   ```bash
   sudo nixos-rebuild switch --flake .#x1-nano
   ```

### Set Up SSH Access

Add your SSH public key to the configuration:

```nix
# In hosts/x1-nano/users.nix
users.users.youruser = {
  isNormalUser = true;
  extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
  openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAA..." ];
};
```

### Configure Wi-Fi

For ThinkPad X1 Nano, Wi-Fi is typically Intel-based:

```nix
# In hosts/x1-nano/system/wifi.nix
networking.wireless = {
  enable = true;
  userControlled.enable = true;
};

networking.wireless.networks."your-ssid" = {
  psk = "your-password";
};
```

### Configure Touchpad

The X1 Nano has a high-quality touchpad. Configure it:

```nix
# In hosts/x1-nano/system/hardware.nix
services.xserver.libinput = {
  enable = true;
  touchpad = {
    naturalScrolling = true;
    tapping = true;
    disableWhileTyping = true;
  };
};
```

### Configure Power Management

Optimize for battery life:

```nix
# In hosts/x1-nano/system/power.nix
powerManagement = {
  powertop.enable = true;
  tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";
    };
  };
};
```

### Configure Sleep/Wake

Ensure proper suspend/resume:

```nix
# In hosts/x1-nano/system/sleep.nix
services.logind = {
  lidSwitch = "suspend";
  lidSwitchExternalPower = "ignore";
  extraConfig = ''
    HandleSuspendKey=suspend
    HandleHibernateKey=hibernate
  '';
};

# Enable sleep target
systemd.targets.suspend = {
  enable = true;
  requires = [ "sleep.target" ];
};
```

## Key Configuration Decisions

### Why LUKS Encryption for Laptop?

Full disk encryption is essential for laptops:
- **Portability**: Laptops are easily lost or stolen
- **Data protection**: Protects sensitive work data
- **Compliance**: Meets security requirements
- **Peace of mind**: Sleep mode doesn't decrypt disk

### Why BTRFS with Subvolumes?

BTRFS provides laptop-specific benefits:
- **Snapshots**: Easy rollback after bad updates
- **Compression**: Saves disk space (important for smaller SSDs)
- **Subvolumes**: Separate system data from user data
- **Copy-on-write**: Data integrity protection

### Why Impermanence?

The `/persist` strategy is ideal for laptops:
- **Clean updates**: System updates don't corrupt user data
- **Easy reinstall**: Just restore from backup
- **Version control**: Configuration in Git
- **Multiple machines**: Same config across devices

### Why Weekly Root Rotation?

Keeping 7 days of snapshots:
- **Update safety**: Rollback after bad updates
- **Testing**: Try new configurations safely
- **Recovery**: Quick restore if something breaks
- **Automatic**: No manual intervention needed

### Why Optional External Swap?

External USB swap for hibernation:
- **Hibernation support**: Save RAM to disk
- **Large RAM**: Useful for machines with 32GB+ RAM
- **Flexibility**: Can be removed if not needed
- **Trade-off**: Slower than internal swap, but functional

## Troubleshooting

### Wi-Fi Issues

If Wi-Fi doesn't work:

```bash
# Check Wi-Fi device
lspci | grep -i network

# Check driver
iwconfig

# Enable Wi-Fi
nmcli device wifi on

# Check logs
journalctl -u NetworkManager
```

### Touchpad Issues

If touchpad doesn't work:

```bash
# Check touchpad device
libinput list-devices

# Test touchpad
evtest /dev/input/eventX

# Check GNOME settings
gsettings get org.gnome.desktop.peripherals.touchpad natural-scroll
```

### Sleep/Wake Issues

If sleep or wake doesn't work:

```bash
# Test sleep
systemctl suspend

# Check sleep logs
journalctl -b -1 | grep -i "sleep\|suspend\|wake"

# Check kernel messages
dmesg | grep -i "acpi\|sleep\|suspend"

# Check logind configuration
loginctl show-session $(loginctl | grep $USER | awk '{print $1}')
```

### Battery Issues

If battery monitoring doesn't work:

```bash
# Check battery status
upower -i /org/freedesktop/UPower/devices/battery_BAT0

# Check AC adapter
upower -i /org/freedesktop/UPower/devices/ACAD

# View battery history
upower -d --battery-history
```

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
   nixos-rebuild switch --flake .#x1-nano
   ```

### SOPS Errors

If SOPS cannot decrypt secrets:

```bash
# Check if keys exist
ls -la /var/lib/sops/keys.txt

# If missing, fetch from Bitwarden manually
bw get item 'sops-age-keys-x1-nano' | jq -r .notes > /var/lib/sops/keys.txt
chmod 600 /var/lib/sops/keys.txt
```

## Maintenance

### Regular Updates

```bash
# System update
sudo nixos-rebuild switch --flake .#x1-nano

# Home Manager update (if applicable)
home-manager switch --flake .#yourusername@x1-nano
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

### Monitor Battery Health

```bash
# Check battery health
upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep capacity

# Check battery cycles (if supported)
upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep cycle-count
```

### Monitor Disk Health

```bash
# SMART data
smartctl -a /dev/nvme0n1

# BTRFS health
btrfs filesystem usage /
btrfs scrub start /
```

### Clean Nix Store

```bash
# Clean old generations
nix-collect-garbage -d

# Optimize store
nix-store --optimise
```

## Laptop-Specific Tips

### Optimize for Travel

```bash
# Enable auto-lock on lid close
gsettings set org.gnome.desktop.screensaver lock-enabled true
gsettings set org.gnome.desktop.screensaver lock-delay 0

# Enable full disk encryption on sleep
sudo systemctl enable fscrypt-lock-on-sleep.service
```

### Dock Configuration

If using a USB-C dock:

```nix
# In hosts/x1-nano/system/dock.nix
services.displayManager.autoLogin = {
  enable = false;
  user = "youruser";
};

# Enable USB-C power delivery
services.udev.packages = [ pkgs.usb-c-dock-config ];
```

### Keyboard Shortcuts

Customize keyboard shortcuts for productivity:

```nix
# In home/x1-nano/configuration.nix
services.xserver.xkb = {
  layout = "us";
  variant = "";
};

# Custom shortcuts
services.xserver.xkbOptions = "compose:menu";
```

## Next Steps

1. **Install development tools**: Set up IDEs, compilers, etc.
2. **Configure Docker**: Install Docker for container development
3. **Set up sync**: Configure Dropbox, OneDrive, or similar
4. **Add productivity tools**: Install communication, note-taking apps
5. **Configure backup**: Set up automated Restic backups

## References

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [ThinkPad X1 Nano Support](https://help.lenovo.com/us/en/solutions/ht511924-thinkpad-x1-nano-gen-1-2-3-support)
- [NixOS Laptop Guide](https://wiki.nixos.org/wiki/Laptops)
- [BTRFS Documentation](https://btrfs.wiki.kernel.org/index.php/Main_Page)
- [SOPS Documentation](https://github.com/getsops/sops)
- [Restic Documentation](https://restic.readthedocs.io/)