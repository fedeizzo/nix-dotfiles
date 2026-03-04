# Setting Up macOS (aarch64-darwin)

This guide walks through setting up a new Mac with NixOS using `nix-darwin` and `home-manager`. This setup is for Apple Silicon Macs (M1, M2, M3) and uses the standard macOS installation with Nix as a layer on top.

## Prerequisites

### Hardware Requirements
- **Mac**: Apple Silicon (M1, M2, M3, or later)
- **RAM**: 8GB minimum, 16GB+ recommended
- **Storage**: Minimum 256GB SSD
- **Network**: Wi-Fi or Ethernet connectivity

### Software Requirements
- **macOS**: macOS 12 (Monterey) or later recommended
- **Xcode Command Line Tools**: Required for building packages
- **Bitwarden Account**: For secrets management (optional)
- **Backblaze B2 Account**: For backups (optional)

### Tools to Install First

1. **Install Xcode Command Line Tools**:
   ```bash
   xcode-select --install
   ```

2. **Install Homebrew** (optional, for non-Nix tools):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

3. **Install Git** (if not already installed):
   ```bash
   sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_*.pkg -target /
   ```

## Overview of Setup

This installation creates:
- **Nix installation** alongside standard macOS
- **Home Manager** for user configuration
- **nix-darwin** for system configuration
- **SOPS integration** for secrets management
- **Restic backup** integration (optional)

### Architecture

```
macOS (base system)
├── /nix (Nix store)
│   ├── /nix/store (package store)
│   └── /nix/var (Nix variables)
├── /etc/nixos (NixOS configuration)
└── ~/ (Home Manager configuration)
    ├── .nix-profile (user profile)
    └── .config/ (user configuration)
```

### Key Differences from Linux Setup

- **No Disko**: Uses native macOS disk setup
- **No LUKS**: Uses macOS FileVault for encryption
- **No Impermanence**: macOS handles persistence natively
- **Different package manager**: `nix-darwin` instead of `nixos-rebuild`
- **Different services**: macOS-specific services (LaunchAgents, etc.)

## Step-by-Step Installation

### 1. Install Base macOS

1. **Boot into Recovery Mode**: Hold `Power` button until "Loading startup options" appears
2. **Select macOS Recovery**: Choose "Reinstall macOS"
3. **Install macOS**: Follow the on-screen instructions
4. **Complete Setup Assistant**: Skip or complete as preferred

**Note**: You can skip creating a user during setup - the Nix configuration will handle user creation.

### 2. Install Xcode Command Line Tools

```bash
xcode-select --install
```

Accept the license agreement when prompted.

### 3. Install Nix

Use the official Nix installer for macOS:

```bash
curl -L https://nixos.org/nix/install | sh
```

During installation:
- Accept default installation path (`/nix`)
- Enable multi-user installation when prompted (recommended)
- Add Nix to your shell profile (recommended)

After installation, reload your shell:
```bash
exec "$SHELL" -l
```

### 4. Verify Nix Installation

```bash
# Check Nix version
nix --version

# Test Nix command
nix eval --expr '1 + 1'
```

You should see `2` as output.

### 5. Clone the Dotfiles Repository

```bash
cd ~
git clone https://github.com/your-org/nix-dotfiles.git
cd nix-dotfiles
```

### 6. Configure the macOS System

Edit the macOS configuration:

```bash
nano hosts/macbook-pro/system/configuration.nix
```

Key configuration options:
- **Username**: Your macOS username
- **SSH keys**: For remote access
- **Hostname**: Your Mac's hostname
- **Services**: Enable/disable macOS services

### 7. Set Up Secrets (Optional)

If using SOPS and Bitwarden:

```bash
# Install Bitwarden CLI
brew install bitwarden-cli

# Login to Bitwarden
bw login

# Unlock vault
bw unlock
```

### 8. Deploy the Configuration

Build and apply the Nix configuration:

```bash
# Build the configuration
nix build .#darwinConfigurations.yourmacname.config

# Apply the configuration
sudo darwin-rebuild switch --flake .#yourmacname
```

**Note**: You may be prompted for your sudo password.

### 9. Set Up Home Manager

Home Manager manages your user configuration:

```bash
# Build Home Manager configuration
nix build .#homeConfigurations.yourusername@yourmacname

# Apply Home Manager configuration
home-manager switch --flake .#yourusername@yourmacname
```

### 10. Verify Installation

Check that Nix packages are available:

```bash
# Check if Nix packages are in PATH
which git

# Check Nix profile
nix profile list

# Check Home Manager packages
home-manager generations
```

## Post-Installation Configuration

### Configure Git

Set up your Git user information:

```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

### Set Up SSH Keys

Generate SSH keys if you don't have them:

```bash
ssh-keygen -t ed25519 -C "your.email@example.com"
```

Add the public key to your GitHub/GitLab account.

### Configure macOS Settings

Some macOS-specific settings can be configured through Home Manager:

```nix
# In home/macbook-pro/configuration.nix
homebrew = {
  enable = true;
  onActivation.autoupdate = true;
  casks = [
    "google-chrome"
    "firefox"
    "visual-studio-code"
  ];
};

# macOS settings
security.loginwindow.TextWelcomeMessage = "Welcome to your Mac";
```

### Set Up Automatic Updates

Configure Nix to auto-update:

```nix
# In hosts/macbook-pro/system/configuration.nix
nix.settings.auto-optimize = true;

# Or add to your shell profile
# ~/.nix-profile/etc/profile.d/nix.sh
```

### Configure Backup (Optional)

If using Restic and Backblaze B2:

```bash
# Set up Restic repository
restic init --repo b2:your-bucket

# Add backup cron job
crontab -e
# Add: 0 2 * * * cd ~/nix-dotfiles && ./scripts/backup.sh
```

## Key Configuration Decisions

### Why Nix on macOS?

Benefits of Nix on macOS:
- **Reproducible**: Same environment across machines
- **Isolated**: Packages don't conflict with system tools
- **Rollback**: Easy to revert changes
- **Consistency**: Same package manager on all platforms

### Why Not Replace macOS?

Keeping macOS as the base:
- **Hardware support**: Better driver support for Mac hardware
- **UI/UX**: Native macOS experience
- **Compatibility**: Run macOS-specific applications
- **Stability**: Proven, stable base system

### Why Multi-user Nix?

Multi-user installation:
- **System-wide**: Available to all users
- **Centralized**: Single Nix store
- **Managed**: Can be administered centrally
- **Stable**: Less prone to permission issues

### Why Home Manager?

Home Manager for user configuration:
- **Declarative**: User config in version control
- **Reproducible**: Same setup on all machines
- **Modular**: Easy to share and reuse
- **Integrated**: Works with nix-darwin

## Troubleshooting

### Nix Installation Issues

If Nix installation fails:

```bash
# Check permissions
ls -la /nix

# Check logs
tail -n 50 /var/log/nix/install.log

# Reinstall
sudo rm -rf /nix
curl -L https://nixos.org/nix/install | sh
```

### Darwin-rebuild Errors

If `darwin-rebuild` fails:

```bash
# Check nix-darwin installation
which darwin-rebuild

# Reinstall nix-darwin
nix-env -iA nix-darwin.darwin

# Build with verbose output
sudo darwin-rebuild switch --flake .#yourmacname --verbose
```

### Home Manager Errors

If Home Manager fails:

```bash
# Check Home Manager installation
which home-manager

# Build with verbose output
home-manager switch --flake .#yourusername@yourmacname --verbose

# Check for conflicts
home-manager generations
```

### Package Build Errors

If packages fail to build:

```bash
# Check Xcode tools
xcode-select -p

# Clean build cache
nix-collect-garbage -d

# Try building with more verbosity
nix-build -A yourpackage --verbose
```

### Path Issues

If Nix packages aren't in PATH:

```bash
# Check if Nix profile is sourced
cat ~/.zprofile | grep nix

# Add to shell profile if missing
echo 'source /etc/profiles/per-user/$USER/profile' >> ~/.zprofile

# Reload shell
exec "$SHELL" -l
```

### Disk Space Issues

If disk runs out of space:

```bash
# Check disk usage
df -h /nix

# Clean Nix store
nix-collect-garbage -d

# Remove old generations
nix-collect-garbage -d --delete-old

# Remove old Home Manager generations
home-manager generational gc
```

### FileVault Conflicts

If FileVault causes issues:

```bash
# Check FileVault status
fdesetup isactive

# FileVault is generally compatible with Nix
# If issues occur, try restarting macOS
```

## Maintenance

### Regular Updates

```bash
# Update system configuration
sudo darwin-rebuild switch --flake .#yourmacname

# Update user configuration
home-manager switch --flake .#yourusername@yourmacname

# Update Homebrew (if enabled)
brew update && brew upgrade
```

### Clean Nix Store

```bash
# Clean old generations
nix-collect-garbage -d

# Optimize store
nix-store --optimise

# Check store usage
du -sh /nix/store
```

### Monitor System Health

```bash
# Check Nix store size
du -sh /nix/store

# Check Home Manager generations
home-manager generations

# Check disk usage
df -h
```

### Backup Configuration

```bash
# Backup Nix configuration
tar -czf nix-config-backup.tar.gz ~/nix-dotfiles

# Backup Home Manager configuration
tar -czf home-config-backup.tar.gz ~/.config/nixpkgs

# Use Restic if configured
restic backup ~/nix-dotfiles
```

## Performance Tips

### Optimize Nix Store

```nix
# In configuration.nix
nix.settings.auto-optimize = true;
nix.settings.optimise.automatic = true;
```

### Enable Caching

```nix
# In configuration.nix
nix.settings.substituters = [
  "https://cache.nixos.org/"
  # Add your own cache if you have one
];
```

### Reduce Build Time

```nix
# Use binary caches
nix.settings.builders-use-substitutes = true;

# Limit parallel builds if needed
nix.settings.max-jobs = "auto";
```

## Next Steps

1. **Install development tools**: Set up Xcode, Homebrew packages
2. **Configure IDEs**: Set up VS Code, IntelliJ, etc.
3. **Set up Docker**: Install Docker Desktop or colima
4. **Configure sync**: Set up iCloud, Dropbox, or similar
5. **Add automation**: Set up LaunchAgents for custom tasks

## References

- [Nix for macOS](https://nixos.org/download.html#macos-install)
- [nix-darwin Documentation](https://nix-darwin.org/)
- [Home Manager Documentation](https://github.com/nix-community/home-manager)
- [macOS System Configuration](https://nixos.org/manual/nixos/stable/#sec-os-config)