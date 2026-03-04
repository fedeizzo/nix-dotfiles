# Setting Up Raspberry Pi (aarch64 Linux)

This guide walks through setting up a Raspberry Pi with NixOS using the homelab configuration. This setup is optimized for Raspberry Pi hardware with a simpler filesystem layout (no encryption) and specific use cases like PiKVM and USB ISO storage.

## Prerequisites

### Hardware Requirements
- **Raspberry Pi**: Pi 4, Pi 5, or compatible (ARM64/aarch64)
- **Storage**: 
  - MicroSD card (minimum 32GB, recommended 64GB+) for OS
  - External USB SSD (minimum 500GB recommended) for data
- **RAM**: 4GB minimum
- **Network**: Ethernet (recommended) or Wi-Fi

### Software Requirements
- **Raspberry Pi Imager**: Download from [raspberrypi.com](https://www.raspberrypi.com/software/)
- **NixOS ARM Image**: Download from [nixos.org](https://nixos.org/download.html#nixos-arm)
- **Bitwarden Account**: For secrets management (optional)
- **Backblaze B2 Account**: For backups (optional)

### Tools Needed
The Raspberry Pi setup uses the NixOS installer image which includes all necessary tools.

## Overview of Setup

This installation creates:
- **ext4 filesystem** (simpler than BTRFS for ARM)
- **No encryption** (simpler setup for always-on server)
- **Multiple data partitions** for specific use cases
- **PiKVM persistence** storage
- **USB ISO storage** for PiKVM virtual media

### Filesystem Layout

```
/dev/sda (External USB SSD)
├── boot (1GB, VFAT)           → /boot (EFI)
├── pimsd (100GB, ext4)        → /var/lib/pimsd (USB ISOs)
├── pipst (10GB, ext4)         → /var/lib/pipst (PiKVM persistence)
└── root (remaining, ext4)     → / (root filesystem)
```

### Use Cases

This configuration is designed for:
- **PiKVM server**: Remote KVM over IP
- **Media server**: Storage for ISOs and media
- **Homelab server**: General-purpose Linux server
- **Always-on device**: No encryption for faster boot

## Step-by-Step Installation

### 1. Prepare the Raspberry Pi

1. **Insert MicroSD card** with NixOS ARM image
2. **Boot the Raspberry Pi**
3. **Connect via SSH**:
   ```bash
   # Find the Pi on your network
   nmap -sn 192.168.1.0/24
   
   # SSH into the Pi (default credentials may vary)
   ssh root@<pi-ip-address>
   ```

### 2. Clone the Dotfiles Repository

```bash
cd /root
git clone https://github.com/your-org/nix-dotfiles.git
cd nix-dotfiles
```

### 3. Prepare the External SSD

Connect your external USB SSD to the Raspberry Pi. Identify the device:

```bash
lsblk
```

Look for a large USB device (e.g., `/dev/sda`). **Note the device name** - you'll need it for the installation.

### 4. Run the Installation Script

```bash
./scripts/installation/raspberry.sh
```

The script will:

#### 4.1 Bitwarden Integration (Optional)
- If configured, fetches SOPS keys and backup credentials from Bitwarden
- Same process as the desktop setup

#### 4.2 Disk Setup with Disko
The script runs Disko to partition the external SSD:

```bash
nix run github:nix-community/disko#disko-install \
  --flake '.#raspberry' \
  --disk main /dev/sda
```

**Important**: This will **erase all data** on the external SSD. Ensure you have the correct device selected.

The disko configuration creates:
- 1GB EFI System Partition for boot
- 100GB partition for PiKVM ISOs (`/var/lib/pimsd`)
- 10GB partition for PiKVM persistence (`/var/lib/pipst`)
- Remaining space for root filesystem

#### 4.3 Backup Restoration (Optional)
If you have Restic backups configured:
- Restores latest snapshot to the system
- Populates the PiKVM and ISO partitions

### 5. Verify the Installation

Check that the filesystem is mounted correctly:
```bash
mount
df -h
```

You should see:
- `/boot` mounted from the ESP
- `/var/lib/pimsd` mounted from the ISO partition
- `/var/lib/pipst` mounted from the PiKVM partition
- `/` mounted from the root partition

### 6. Configure the System

Edit the configuration for your specific needs:

```bash
# Edit the Raspberry Pi configuration
nano nix/nixosConfigurations.nix
```

Update the `raspberry` configuration with:
- Your hostname
- SSH keys for access
- Network configuration
- Any additional services (PiKVM, media server, etc.)

### 7. Reboot

```bash
reboot
```

The system will reboot from the external SSD.

## Post-Installation Configuration

### Set Up SSH Access

Add your SSH public key to the configuration:

```nix
# In hosts/raspberry/users.nix
users.users.root = {
  openssh.authorizedKeys.keys = [ "ssh-rsa AAAA..." ];
};

# Or create a regular user
users.users.youruser = {
  isNormalUser = true;
  extraGroups = [ "wheel" ];
  openssh.authorizedKeys.keys = [ "ssh-rsa AAAA..." ];
};
```

### Configure Network

For static IP configuration:

```nix
# In hosts/raspberry/system/network.nix
networking = {
  hostName = "raspberry";
  networkmanager.enable = true;
  
  # Or manual configuration
  interfaces.eth0 = {
    useDHCP = false;
    ipv4.addresses = [
      {
        address = "192.168.1.100";
        prefixLength = 24;
      }
    ];
    ipv4.routes = [
      {
        address = "0.0.0.0";
        prefixLength = 0;
        gateway = "192.168.1.1";
      }
    ];
  };
};
```

### Set Up PiKVM

If using this as a PiKVM server:

```nix
# Install PiKVM packages
environment.systemPackages = with pkgs; [
  pikvm
];

# Enable PiKVM service
services.pikvm.enable = true;
```

### Configure Wi-Fi (if using Wi-Fi)

```nix
# In hosts/raspberry/system/wifi.nix
networking.wireless = {
  enable = true;
  userControlled.enable = true;
};

networking.wireless.networks."your-ssid" = {
  psk = "your-password";
};
```

## Key Configuration Decisions

### Why ext4 Instead of BTRFS?

For Raspberry Pi:
- **Simpler**: Fewer moving parts, easier to troubleshoot
- **Compatible**: Better compatibility with USB drives
- **Proven**: More mature on ARM platforms
- **Performance**: Comparable performance for this use case

### Why No Encryption?

For a homelab server:
- **Always-on**: No need to enter passphrase on boot
- **Physical security**: Server in controlled environment
- **Performance**: No encryption overhead
- **Simplicity**: Fewer points of failure

### Why Separate Partitions?

- **PiKVM ISOs** (`/var/lib/pimsd`): Large storage for virtual media
- **PiKVM persistence** (`/var/lib/pipst`): Configuration and state
- **Isolation**: Each partition can be managed independently
- **Backup flexibility**: Selective backup of specific data

### Why 1GB Boot Partition?

- **EFI standard**: Meets UEFI requirements
- **Future-proof**: Space for multiple kernels
- **Safety**: Separate from data partitions

## Troubleshooting

### Pi Won't Boot from SSD

1. **Check boot order**: Ensure USB boot is enabled in Raspberry Pi BIOS
2. **Verify ESP**: Check that the EFI partition is properly formatted
3. **Check cables**: Use high-quality USB 3.0 cables
4. **Power supply**: Ensure adequate power (5V/5A recommended for Pi 4/5)

### SSH Connection Issues

```bash
# Check SSH service
sudo systemctl status sshd

# View logs
sudo journalctl -u sshd

# Check firewall
sudo nft list ruleset
```

### Disk Space Issues

```bash
# Check disk usage
df -h

# Clean Nix stores
sudo nix-collect-garbage -d

# Remove old generations
sudo nixos-rebuild gc
```

### PiKVM Issues

```bash
# Check PiKVM service
sudo systemctl status pikvm

# Check web interface
curl http://localhost:8080

# View logs
sudo journalctl -u pikvm
```

### USB Drive Not Detected

```bash
# Check USB connectivity
lsusb

# Check kernel messages
dmesg | grep -i usb

# Check if drive is recognized
lsblk
```

### Network Configuration Issues

```bash
# Check network status
ip addr

# Test connectivity
ping -c 4 google.com

# Check DNS
cat /etc/resolv.conf

# Test DNS resolution
nslookup google.com
```

## Maintenance

### Regular Updates

```bash
# SSH into the Pi
ssh root@<pi-ip-address>

# Update the system
cd /root/nix-dotfiles
sudo nixos-rebuild switch --flake .#raspberry
```

### Monitor Disk Health

```bash
# Check SMART data (if supported)
smartctl -a /dev/sda

# Check filesystem
sudo fsck.ext4 -n /dev/sda3
```

### Monitor Services

```bash
# Check all services
systemctl list-units --type=service

# Check specific services
systemctl status pikvm
```

### Backup Configuration

```bash
# Create manual backup
restic backup /var/lib/pipst

# Check backup status
restic snapshots
```

## Performance Optimization

### Enable ZRAM (if low RAM)

```nix
# In hosts/raspberry/system/performance.nix
zramSwap.enable = true;
```

### Optimize for SSD

```nix
# In hosts/raspberry/system/disko.nix
# Add SSD optimizations to the root partition
content = {
  type = "filesystem";
  format = "ext4";
  mountOptions = [
    "noatime"
    "discard=async"
  ];
  mountpoint = "/";
};
```

### CPU Scaling

```nix
# In hosts/raspberry/system/performance.nix
powerManagement.cpuFreqGovernor = "performance";
```

## Next Steps

1. **Set up PiKVM**: Configure remote KVM access
2. **Add media server**: Install Jellyfin, Plex, or similar
3. **Configure backups**: Set up automated Restic backups
4. **Add monitoring**: Install Prometheus, Grafana, or Netdata
5. **Set up Docker**: Install container runtime for additional services

## References

- [Raspberry Pi Documentation](https://www.raspberrypi.com/documentation/)
- [NixOS ARM Installation](https://nixos.org/manual/nixos/stable/#sec-installation-arm)
- [PiKVM Documentation](https://docs.pikvm.org/)
- [Disko Documentation](https://github.com/nix-community/disko)