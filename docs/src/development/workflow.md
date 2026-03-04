# Development Workflow Guide

This guide covers the essential workflows for developing and maintaining the NixOS dotfiles configuration.

## Table of Contents

- [Adding New Services](#adding-new-services)
  - [NixOS Services](#nixos-services)
  - [Home Manager Services](#home-manager-services)
- [Testing Changes](#testing-changes)
  - [Local Testing](#local-testing)
  - [Remote Testing](#remote-testing)
- [Keeping the System Updated](#keeping-the-system-updated)
- [Debugging Broken Configurations](#debugging-broken-configurations)
- [Secrets Management](#secrets-management)

---

## Adding New Services

### NixOS Services (Homelab)

The homelab uses a custom service module system in `hosts/framework-desktop/services/`. Services are defined as a list in `module.nix` with options for reverse proxy, persistence, and backup.

#### Service Module Structure

Services use the `fi.services` list with these options:
- `name` - Service identifier
- `subdomain` - Reverse proxy domain (defaults to name)
- `port` - Service port
- `path` - URL path prefix (optional)
- `isExposed` - Enable reverse proxy exposure
- `authType` - Authentication type ("proxy" or "none")
- `dashboardSection` - Dashboard categorization
- `dashboardIcon` - Dashboard icon
- `toPersist` - Directories to persist across reboots
- `toBackup` - Directories to backup with restic

#### Creating a New Service

1. **Create service directory** in `hosts/framework-desktop/services/`:
   ```bash
   mkdir hosts/framework-desktop/services/<service-name>
   ```

2. **Create `default.nix`** with the service module:
   ```nix
   { config, pkgs, lib, fi, ... }:

   let
     cfg = config.fi.services.<service-name>;
   in

   {
     options.fi.services.<service-name> = {
       enable = lib.mkEnableOption "<service-name>";

       # Service-specific configuration
       port = lib.mkOption {
         type = lib.types.port;
         default = 8080;
         description = "Service port";
       };
     };

     config = lib.mkIf cfg.enable {
       # Enable the NixOS service
       services.<service-name> = {
         enable = true;
         # Service-specific configuration
       };

       # Persist service data
       fileSystems."<persist-path>" = {
         device = "/persist/<persist-path>";
         fsType = "none";
         options = [ "bind" ];
       };
     };
   }
   ```

3. **Register the service** in `hosts/framework-desktop/services/module.nix`:
   ```nix
   { pkgs, config, lib, ... }:
   with lib;
   let
     cfg = config.fi;
   in
   {
     options.fi = {
       services = mkOption {
         type = types.listOf (types.submodule ({ config, ... }: {
           options = {
             name = mkOption { type = types.str; };
             subdomain = mkOption { type = types.nullOr types.str; default = config.name; };
             port = mkOption { type = types.int; };
             path = mkOption { type = types.str; default = ""; };
             isExposed = mkOption { type = types.bool; default = false; };
             authType = mkOption { type = (types.enum [ "proxy" "none" ]); default = "none"; };
             dashboardSection = mkOption { type = types.str; };
             dashboardIcon = mkOption { type = types.str; default = "${config.name}"; };
             toPersist = mkOption { type = types.listOf (types.submodule { options = {
               directory = mkOption { type = types.str; };
               user = mkOption { type = types.nullOr types.str; };
               group = mkOption { type = types.nullOr types.str; };
               mode = mkOption { type = types.nullOr types.str; };
             }; }); default = [ ]; };
             toBackup = mkOption { type = types.listOf types.str; default = [ ]; };
           };
         }));
       };
     };
   }
   ```

4. **Enable the service** in `hosts/framework-desktop/system/default.nix`:
   ```nix
   fi.services.<service-name> = {
     enable = true;
     port = 8080;
     subdomain = "<service>";
     isExposed = true;
     authType = "proxy";
     dashboardSection = "monitoring";
     toPersist = [
       {
         directory = "/var/lib/<service>";
         user = "<service>";
         group = "<service>";
       }
     ];
     toBackup = [ "/var/lib/<service>" ];
   };
   ```

#### Example: Adding a New Service

See `hosts/framework-desktop/services/uptime-kuma/` for a complete example of a service with Traefik configuration.

#### Common Service Patterns

**Service with persistence:**
```nix
# Persist data across reboots (impermanence)
fileSystems."/var/lib/<service>" = {
  device = "/persist/var/lib/<service>";
  fsType = "none";
  options = [ "bind" ];
};
```

**Service with Traefik:**
```nix
# Enable reverse proxy
traefik = {
  enable = true;
  domain = "<service>.local";
};
```

**Service without reverse proxy:**
```nix
# Just enable the NixOS service
services.<service-name>.enable = true;
```

### Home Manager Services

Home Manager configurations are in `home/` directory. Each host has its own configuration that imports common modules.

#### Adding a Home Manager Package

1. **Edit the host configuration** (e.g., `home/x1-nano/default.nix`):
   ```nix
   { pkgs, ... }:

   {
     home.packages = with pkgs; [
       # ... existing packages
       <new-package>
     ];
   }
   ```

2. **Enable the Home Manager service** in the NixOS configuration:
   ```nix
   # In hosts/<host>/configuration.nix
   programs.home-manager.enable = true;
   ```

#### Creating a Home Manager Module

For reusable configurations:

1. **Create module in `home/common/`**:
   ```nix
   # home/common/<module-name>/default.nix
   { config, lib, pkgs, ... }:

   let
     cfg = config.<module-name>;
   in

   {
     options.<module-name> = {
       enable = lib.mkEnableOption "<module-name>";
     };

     config = lib.mkIf cfg.enable {
       # Module configuration
     };
   }
   ```

2. **Import the module** in the host configuration:
   ```nix
   {
     imports = [
       ./common/<module-name>
     ];
   }
   ```

---

## Testing Changes

### Local Testing

#### Using NH (Nix Home)

NH is the recommended tool for local configuration testing and deployment.

**Build configuration without applying:**
```bash
nh os build
```

**Switch to the new configuration:**
```bash
nh os switch
```

**Dry-run (preview changes):**
```bash
nh os switch --dry-run
```

**Rollback to previous generation:**
```bash
nh os switch --rollback
```

**List generations:**
```bash
nh os list
```

**Clean up old generations:**
```bash
nh os clean --keep 5
```

#### Using NixOS Reconfigure (Traditional)

```bash
# Build and switch
sudo nixos-rebuild switch --flake .#<hostname>

# Test without applying
sudo nixos-rebuild build --flake .#<hostname>

# Switch to specific generation
sudo nixos-rebuild switch --flake .#<hostname> --generation <id>
```

### Remote Testing

#### Deploy to Remote Host

The Framework Desktop is configured for remote deployment with deploy-rs.

**Deploy to homelab (Framework Desktop):**
```bash
nh deploy homelab
```

**Deploy with dry-run:**
```bash
nh deploy homelab --dry-run
```

**Deploy specific host:**
```bash
nh deploy <hostname>
```

#### Understanding Remote Deployment

The deployment configuration is in `nix/deployment.nix`. Key features:

- **Remote builds**: Avoids 50-100GB network transfers by building on the remote host
- **Atomic deployments**: Each deployment creates a new generation
- **Rollback support**: Magic rollback available in boot menu if system fails to boot

**Deploy nodes configured:**
- `homelab` - Framework Desktop (192.168.1.100)

#### SSH Key Setup

Remote deployment requires SSH access. The SSH key is managed by SOPS:
```bash
# Check if SSH key is available
ls -la hosts/framework-desktop/system/ssh-deploy-key

# If using age keys, ensure they're in your key agent
ssh-add ~/.ssh/id_ed25519
```

---

## Keeping the System Updated

### Updating Nix Flakes

```bash
# Update all flakes
nix flake update

# Update specific flake
nix flake update nixpkgs

# Check for updates without applying
nix flake update --dry-run
```

### Updating System Packages

**NixOS System:**
```bash
# Update system packages
sudo nixos-rebuild switch --upgrade

# Update only NixOS version
sudo nixos-rebuild switch --upgrade nixpkgs

# Update all inputs
sudo nixos-rebuild switch --upgrade --upgrade-all
```

**Home Manager:**
```bash
# Update home manager packages
home-manager switch --upgrade

# Update home manager with flake
home-manager switch --flake .#<hostname> --upgrade
```

### Using NH for Updates

```bash
# Update system and packages
nh os switch --upgrade

# Update only NixOS
nh os switch --upgrade nixpkgs

# Update home manager
nh hm switch --upgrade
```

### Best Practices

1. **Update frequently**: Keep nixpkgs updated to avoid large upgrade jumps
2. **Test before deploying**: Always test locally before remote deployment
3. **Keep generations**: Don't clean up generations until confident the update works
4. **Check changelogs**: Review nixpkgs changelog for breaking changes
5. **Backup secrets**: Ensure SOPS secrets are backed up before major updates

---

## Debugging Broken Configurations

### System Won't Boot

#### Accessing Previous Generation

1. **Reboot the system**
2. **Enter GRUB boot menu** (hold Shift or press Esc during boot)
3. **Select "NixOS"** → "Advanced options"
4. **Choose previous working generation**
5. **Boot from selected generation**

#### Magic Rollback

If magic rollback is enabled (default):
```bash
# After boot failure, press any key when prompted
# System will automatically rollback to previous generation
```

### Configuration Build Errors

#### Getting Detailed Error Messages

```bash
# Build with full error output
nh os build --verbose

# Build with debug information
nix-build -A nixosConfigurations.<hostname>.config.system.build.toplevel --show-trace

# Check specific module
nix-instantiate --eval -E '(import <nixpkgs/nixos> {}).lib.modulesystem.runModuleSystemTests {}'
```

#### Common Build Errors

**Module option errors:**
```bash
# Check available options
nixos-option <option-name>

# Check option documentation
nixos-option <option-name> --doc
```

**Dependency errors:**
```bash
# Check if package exists
nix search <package-name>

# Check package attributes
nix eval nixpkgs#<package-name>
```

### Service Issues

#### Checking Service Status

```bash
# Check systemd service
systemctl status <service-name>

# View service logs
journalctl -u <service-name> -f

# Restart service
sudo systemctl restart <service-name>
```

#### Testing Service Configuration

```bash
# Check if service is enabled
systemctl is-enabled <service-name>

# Check service configuration
systemctl cat <service-name>

# Test service start
sudo systemctl start <service-name>
```

#### Traefik Debugging

```bash
# Check Traefik logs
journalctl -u traefik -f

# Test Traefik configuration
sudo traefik traefik.yml --check

# View active routers
curl http://localhost:8000/api/http/routers
```

### Network Issues

#### Testing Connectivity

```bash
# Test SSH to remote host
ssh -v user@192.168.1.100

# Test reverse proxy
curl -H "Host: <service>.local" http://localhost

# Check firewall rules
sudo nft list ruleset
```

### Secrets Issues

#### Decrypting Secrets Locally

```bash
# Decrypt a secret file
sops decrypt hosts/<host>/system/<secret-file>.yaml.enc

# Check SOPS age key
sops age --show-key hosts/<host>/system/age-key.txt
```

#### Common Secrets Errors

**Permission denied:**
```bash
# Fix permissions on decrypted file
chmod 600 /etc/secrets/<secret-file>
```

**Key not found:**
```bash
# Verify age key is correct
cat hosts/<host>/system/age-key.txt

# Check SOPS configuration
cat .sops.yaml
```

### Using Nix Search for Packages

```bash
# Search for package
nix search nixpkgs <package>

# Get package info
nix show <package>

# Check package dependencies
nix show <package> | grep -A 20 "depends:"
```

### Common Pitfalls and Solutions

**Issue**: Configuration builds but service doesn't start
- **Solution**: Check service dependencies and ensure persist directories exist

**Issue**: Traefik returns 502 Bad Gateway
- **Solution**: Verify backend service is running and port matches configuration

**Issue**: Remote deployment fails
- **Solution**: Check SSH connectivity and ensure deploy-rs credentials are correct

**Issue**: Home Manager configuration conflicts
- **Solution**: Check for duplicate option definitions and ensure proper module ordering

---

## Secrets Management

### Overview

Secrets are encrypted using SOPS with age encryption. Each host has its own secrets file.

**Secrets location:**
- `hosts/<host>/system/<secret-file>.yaml.enc`
- `hosts/<host>/home/<secret-file>.yaml.enc`

**Encryption:**
- Algorithm: age
- Keys: Stored in password manager (Bitwarden/rbw)
- Files: Encrypted in place (`.yaml.enc` extension)

### Editing Secrets

#### Using the Edit Script (Recommended)

```bash
# Edit secrets for specific host
./scripts/edit_secrets.sh <host>

# Example: Edit laptop secrets
./scripts/edit_secrets.sh x1-nano

# Example: Edit homelab secrets
./scripts/edit_secrets.sh framework-desktop
```

The script will:
1. Decrypt the secrets file
2. Open in editor (defaults to `vim`)
3. Re-encrypt when closed
4. Verify the encryption

#### Manual Editing

```bash
# Decrypt to temporary file
sops decrypt hosts/<host>/system/oven-secrets.yaml.enc > /tmp/secrets.yaml

# Edit the decrypted file
vim /tmp/secrets.yaml

# Re-encrypt
sops --encrypt --age <age-key> --input-type yaml --output-type yaml /tmp/secrets.yaml > hosts/<host>/system/oven-secrets.yaml.enc

# Replace original
mv hosts/<host>/system/oven-secrets.yaml.enc hosts/<host>/system/oven-secrets.yaml.enc.bak
cp /tmp/secrets.yaml hosts/<host>/system/oven-secrets.yaml.enc
```

### Adding New Secrets

#### 1. Add to SOPS Configuration

Edit `.sops.yaml` to define encryption keys:
```yaml
creation_rules:
  - path_regex: .*\.yaml\.enc$
    age: "age-1<key1>,age-1<key2>"
```

#### 2. Add Secret to Host File

```yaml
# hosts/<host>/system/oven-secrets.yaml.enc (after decrypting)
my_secret: "secret-value"
database_password: "db-password"
api_key: "api-key-value"
```

#### 3. Reference Secret in Configuration

```nix
# In configuration.nix or service module
sops.secrets.my_secret = {
  source = "my_secret";
};

# Use in service configuration
environment.variables.MY_SECRET = config.secrets.my_secret.path;
```

### Secrets in Service Modules

Services can access secrets through the `fi.secrets` abstraction:

```nix
# In service default.nix
{ config, pkgs, fi, ... }:

{
  # Access secrets
  environment.variables.API_KEY = fi.secrets.api-key.path;
  
  # Or use in service configuration
  services.nginx = {
    enable = true;
    virtualHosts."example.com" = {
      locations."/" = {
        proxyPass = "http://localhost:8080";
        extraConfig = ''
          proxy_set_header X-API-Key ${fi.secrets.api-key.path};
        '';
      };
    };
  };
}
```

### Managing Age Keys

#### Viewing Age Keys

```bash
# View public key
cat hosts/<host>/system/age-key.txt.pub

# View private key (careful!)
cat hosts/<host>/system/age-key.txt
```

#### Adding New Key Recipient

```bash
# Generate new age key pair
age-keygen -o age-key.txt

# Get public key
cat age-key.txt.pub

# Update .sops.yaml with new public key
# Add to password manager
```

#### Rotating Secrets

```bash
# Decrypt secrets
sops decrypt hosts/<host>/system/oven-secrets.yaml.enc > /tmp/secrets.yaml

# Re-encrypt with new keys
sops --encrypt --age "age-1<old-key>,age-1<new-key>" /tmp/secrets.yaml > hosts/<host>/system/oven-secrets.yaml.enc

# Remove old key after verification
# Update .sops.yaml
```

### Common Secrets Issues

**Issue**: "Permission denied" when accessing secrets
- **Solution**: Ensure secret file has correct permissions (600)
- **Solution**: Check that sops-nix is properly configured

**Issue**: "Key not found" error
- **Solution**: Verify age key is in your key agent
- **Solution**: Check that the key matches the recipient in .sops.yaml

**Issue**: Secrets not appearing in environment
- **Solution**: Ensure secrets are properly referenced in configuration
- **Solution**: Check that the service is restarted after secret changes

### Best Practices

1. **Never commit plaintext secrets**: Always keep secrets encrypted
2. **Use separate keys per host**: Different hosts should have different age keys
3. **Rotate keys regularly**: Update age keys every 6-12 months
4. **Backup keys securely**: Store age private keys in password manager
5. **Test secret changes**: Verify secrets are accessible after changes
6. **Document secret usage**: Add comments explaining what each secret is for

### SOPS Configuration

The `.sops.yaml` file defines encryption rules:

```yaml
creation_rules:
  - path_regex: hosts/.*\.yaml\.enc$
    age: "age-1<key1>,age-1<key2>"
    pgp: ""
```

**Key points:**
- `path_regex`: Matches file patterns
- `age`: Comma-separated list of age public keys
- Multiple keys allow multiple hosts to decrypt the same file

### Editing Workflow

```bash
# 1. Edit secrets
./scripts/edit_secrets.sh framework-desktop

# 2. Make changes in editor

# 3. Save and exit (auto-re-encrypts)

# 4. Test configuration
nh os switch --flake .#framework-desktop

# 5. If successful, commit changes
jj commit -m "Add new secret: example"
```

---

## Quick Reference

### Common Commands

```bash
# Local testing
nh os build              # Build configuration
nh os switch            # Switch to new configuration
nh os list              # List generations
nh os rollback          # Rollback to previous

# Remote deployment
nh deploy homelab       # Deploy to homelab
nh deploy homelab --dry-run  # Dry-run deployment

# Updates
nix flake update        # Update flakes
nh os switch --upgrade  # Update and switch

# Secrets
./scripts/edit_secrets.sh <host>  # Edit secrets
sops decrypt <file>.yaml.enc      # Decrypt manually

# Debugging
systemctl status <service>  # Check service
journalctl -u <service>     # View logs
nixos-option <option>       # Check option
```

### File Structure

```
.
├── flake.nix                 # Main flake configuration
├── .sops.yaml               # SOPS encryption rules
├── nix/
│   ├── nixosConfigurations.nix  # NixOS system definitions
│   └── deployment.nix       # Remote deployment config
├── hosts/
│   ├── <host>/
│   │   ├── configuration.nix  # Host NixOS config
│   │   └── system/
│   │       └── *.yaml.enc   # Encrypted secrets
│   └── framework-desktop/
│       └── services/        # Service definitions
└── home/
    └── <host>/              # Home Manager config
```

---

This guide covers the essential workflows for the NixOS dotfiles configuration. For more detailed information, check the individual configuration files and the NixOS and Home Manager documentation.