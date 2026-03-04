# Testing Changes

This guide covers testing NixOS configuration changes locally and remotely.

## Local Testing

### Using NH (Nix Home)

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

### Using NixOS Reconfigure (Traditional)

```bash
# Build and switch
sudo nixos-rebuild switch --flake .#<hostname>

# Test without applying
sudo nixos-rebuild build --flake .#<hostname>

# Switch to specific generation
sudo nixos-rebuild switch --flake .#<hostname> --generation <id>
```

## Remote Testing

### Deploy to Remote Host

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

### Understanding Remote Deployment

The deployment configuration is in `nix/deployment.nix`. Key features:

- **Remote builds**: Avoids 50-100GB network transfers by building on the remote host
- **Atomic deployments**: Each deployment creates a new generation
- **Rollback support**: Magic rollback available in boot menu if system fails to boot

**Deploy nodes configured:**
- `homelab` - Framework Desktop (192.168.1.100)

### SSH Key Setup

Remote deployment requires SSH access. The SSH key is managed by SOPS:
```bash
# Check if SSH key is available
ls -la hosts/framework-desktop/system/ssh-deploy-key

# If using age keys, ensure they're in your key agent
ssh-add ~/.ssh/id_ed25519
```