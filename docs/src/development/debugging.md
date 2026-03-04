# Debugging Broken Configurations

This guide covers debugging common issues with NixOS configurations.

## System Won't Boot

### Accessing Previous Generation

1. **Reboot the system**
2. **Enter GRUB boot menu** (hold Shift or press Esc during boot)
3. **Select "NixOS"** → "Advanced options"
4. **Choose previous working generation**
5. **Boot from selected generation**

### Magic Rollback

If magic rollback is enabled (default):
```bash
# After boot failure, press any key when prompted
# System will automatically rollback to previous generation
```

## Configuration Build Errors

### Getting Detailed Error Messages

```bash
# Build with full error output
nh os build --verbose

# Build with debug information
nix-build -A nixosConfigurations.<hostname>.config.system.build.toplevel --show-trace

# Check specific module
nix-instantiate --eval -E '(import <nixpkgs/nixos> {}).lib.modulesystem.runModuleSystemTests {}'
```

### Common Build Errors

#### Module option errors

```bash
# Check available options
nixos-option <option-name>

# Check option documentation
nixos-option <option-name> --doc
```

#### Dependency errors

```bash
# Check if package exists
nix search <package-name>

# Check package attributes
nix eval nixpkgs#<package-name>
```

## Service Issues

### Checking Service Status

```bash
# Check systemd service
systemctl status <service-name>

# View service logs
journalctl -u <service-name> -f

# Restart service
sudo systemctl restart <service-name>
```

### Testing Service Configuration

```bash
# Check if service is enabled
systemctl is-enabled <service-name>

# Check service configuration
systemctl cat <service-name>

# Test service start
sudo systemctl start <service-name>
```

### Traefik Debugging

```bash
# Check Traefik logs
journalctl -u traefik -f

# Test Traefik configuration
sudo traefik traefik.yml --check

# View active routers
curl http://localhost:8000/api/http/routers
```

## Network Issues

### Testing Connectivity

```bash
# Test SSH to remote host
ssh -v user@192.168.1.100

# Test reverse proxy
curl -H "Host: <service>.local" http://localhost

# Check firewall rules
sudo nft list ruleset
```

## Secrets Issues

### Decrypting Secrets Locally

```bash
# Decrypt a secret file
sops decrypt hosts/<host>/system/<secret-file>.yaml.enc

# Check SOPS age key
sops age --show-key hosts/<host>/system/age-key.txt
```

### Common Secrets Errors

#### Permission denied

```bash
# Fix permissions on decrypted file
chmod 600 /etc/secrets/<secret-file>
```

#### Key not found

```bash
# Verify age key is correct
cat hosts/<host>/system/age-key.txt

# Check SOPS configuration
cat .sops.yaml
```

## Using Nix Search for Packages

```bash
# Search for package
nix search nixpkgs <package>

# Get package info
nix show <package>

# Check package dependencies
nix show <package> | grep -A 20 "depends:"
```

## Common Pitfalls and Solutions

| Issue | Solution |
|-------|----------|
| Configuration builds but service doesn't start | Check service dependencies and ensure persist directories exist |
| Traefik returns 502 Bad Gateway | Verify backend service is running and port matches configuration |
| Remote deployment fails | Check SSH connectivity and ensure deploy-rs credentials are correct |
| Home Manager configuration conflicts | Check for duplicate option definitions and ensure proper module ordering |