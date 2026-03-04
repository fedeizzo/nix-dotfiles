# Deployment Strategy

This document describes how configuration changes are deployed to remote systems using `deploy-rs`.

## Why deploy-rs

`deploy-rs` was chosen over alternatives like `nixos-rebuild` remote mode or `nix-build` + SCP for several reasons:

- **Atomic deployments**: Each deployment creates a new generation in `/nix/var/nix/profiles/system`, enabling instant rollback
- **Remote build support**: Builds occur on the target machine, avoiding large transfer of closure artifacts
- **Integration with flakes**: Native support for flake-based configurations
- **Rollback automation**: `autoRollback` can detect and revert failed deployments
- **Magic rollback**: `magicRollback` enables rollback via boot menu without SSH access

## Configuration Structure

The deployment configuration lives in `nix/deployment.nix`:

```nix
{ inputs, homelab-configuration, ... }:

{
  deploy.nodes = {
    homelab = {
      hostname = "homelab";
      sshUser = "root";
      sudo = "doas -u";
      sshOpts = [ ];
      magicRollback = true;
      autoRollback = true;
      fastConnection = false;
      remoteBuild = true;
      profiles.system = {
        user = "root";
        path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos homelab-configuration;
      };
    };
  };
}
```

### Key Settings

| Setting | Value | Purpose |
|---------|-------|---------|
| `hostname` | "homelab" | SSH hostname (resolves via DNS or /etc/hosts) |
| `sshUser` | "root" | SSH user for deployment |
| `sudo` | "doas -u" | Sudo command (uses doas instead of plain sudo) |
| `magicRollback` | true | Enable rollback via boot menu |
| `autoRollback` | true | Auto-revert on deployment failure |
| `fastConnection` | false | Disable compression for local network |
| `remoteBuild` | true | Build on target machine, not locally |

## Remote Build Strategy

`remoteBuild = true` is critical for the homelab setup:

- **Why**: Building the entire NixOS closure locally and transferring it would require 50-100GB of network transfer
- **How**: The target machine has the same flake inputs (via git submodule or copied flake.lock) and builds locally
- **Requirement**: Target must have Nix with flakes enabled and access to the same inputs

## Deployment Workflow

### Deploying to Homelab

```bash
# Deploy using deploy-cli
deploy-cli deploy '.#homelab'

# Or using nixos-rebuild (wrapper around deploy-cli)
nixos-rebuild switch --target-host root@homelab --flake '.#homelab'
```

### Verifying Deployment

```bash
# Check deployment status
deploy-cli status homelab

# Check generations
ssh root@homelab "nix-env -p /nix/var/nix/profiles/system --list-generations"
```

## Rollback Mechanisms

### Automatic Rollback

If a deployment fails mid-activation, `autoRollback` detects the failure and reverts to the previous generation automatically.

### Manual Rollback via Boot Menu

With `magicRollback = true`, you can boot into previous generations:

1. Reboot the machine
2. Select "Previous NixOS generation" from the boot menu
3. System boots into the previous configuration

### Manual Rollback via SSH

```bash
# List generations
nix-env -p /nix/var/nix/profiles/system --list-generations

# Switch to specific generation
nix-env -p /nix/var/nix/profiles/system --switch-generation <number>

# Or activate specific generation
nixos-rebuild switch --build-host homelab --target-host root@homelab --flake '.#homelab' --generation <number>
```

## Integration with flake-parts

The deployment configuration is imported in `flake.nix`:

```nix
(flake-parts.lib.mkFlake { inherit inputs; } {
  imports = [ ./nix/deployment.nix ];
  
  flake = {
    imports = [
      (import ./nix/deployment.nix {
        inherit inputs;
        homelab-configuration = (import ./nix/nixosConfigurations.nix { inherit inputs; }).nixosConfigurations.homelab;
      });
    ];
    
    checks = builtins.mapAttrs (_system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;
  };
})
```

The `deployChecks` ensure that deployments won't break before actually deploying.

## Security Considerations

### SSH Key Management

- Root SSH access is required for deployment
- SSH keys should be stored in SOPS and deployed via `sops-nix`
- Consider using SSH keys with limited privileges (though root is required for NixOS activation)

### Network Security

- Deployments occur over SSH (encrypted)
- Consider restricting SSH access via firewall to known IPs
- `fastConnection = false` disables SSH compression (saves CPU, not security)

## Troubleshooting

### Deployment Fails to Connect

```bash
# Test SSH connection manually
ssh root@homelab "echo 'Connection successful'"

# Check SSH opts
# Add verbose output: sshOpts = [ "-v" "-i /path/to/key" ];
```

### Build Fails on Remote

- Verify flake inputs are available on target
- Check that target has same Nix version and flakes enabled
- Review build logs via SSH: `journalctl -u nixos-rebuild -f`

### Activation Fails

- Check activation logs: `journalctl -u nixos-activate -b -n 100`
- Rollback automatically happens with `autoRollback = true`
- Manual rollback: `nix-env -p /nix/var/nix/profiles/system --switch-generation -1`

## Comparison with Alternatives

| Tool | Pros | Cons | Used For |
|------|------|------|----------|
| **deploy-rs** | Atomic, rollback, remote build, flake-native | Requires setup, extra dependency | Remote NixOS deployment |
| `nixos-rebuild --target-host` | Built-in, simple | No atomic rollback, transfers large closures | Simple cases |
| `nix-build` + SCP | Full control, simple | Manual, error-prone, no rollback | Custom workflows |
| `home-manager --flake --impure` | Good for user env | Not for system config | Home Manager only |

## Best Practices

1. **Test locally first**: `nixos-rebuild switch --flake '.#homelab'` before remote deployment
2. **Check deployment**: `deploy-cli deploy '.#homelab' --dry-build --dry-activate` before actual deploy
3. **Monitor activation**: Watch logs during first few deployments
4. **Verify generations**: After deployment, confirm new generation exists
5. **Keep backup access**: Ensure SSH access works even if deployment breaks

## Future Considerations

- **Multiple systems**: Extend `deploy.nodes` for rasp-configuration and other machines
- **CI/CD integration**: Consider GitHub Actions for automated deployments
- **Canary deployments**: Deploy to test machine before production
- **Configuration drift**: Regular audits to ensure config matches declared state