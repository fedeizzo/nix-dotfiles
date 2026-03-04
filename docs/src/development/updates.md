# Keeping the System Updated

This guide covers updating Nix flakes and system packages.

## Updating Nix Flakes

```bash
# Update all flakes
nix flake update

# Update specific flake
nix flake update nixpkgs

# Check for updates without applying
nix flake update --dry-run
```

## Updating System Packages

### NixOS System

```bash
# Update system packages
sudo nixos-rebuild switch --upgrade

# Update only NixOS version
sudo nixos-rebuild switch --upgrade nixpkgs

# Update all inputs
sudo nixos-rebuild switch --upgrade --upgrade-all
```

### Home Manager

```bash
# Update home manager packages
home-manager switch --upgrade

# Update home manager with flake
home-manager switch --flake .#<hostname> --upgrade
```

## Using NH for Updates

```bash
# Update system and packages
nh os switch --upgrade

# Update only NixOS
nh os switch --upgrade nixpkgs

# Update home manager
nh hm switch --upgrade
```

## Best Practices

1. **Update frequently**: Keep nixpkgs updated to avoid large upgrade jumps
2. **Test before deploying**: Always test locally before remote deployment
3. **Keep generations**: Don't clean up generations until confident the update works
4. **Check changelogs**: Review nixpkgs changelog for breaking changes
5. **Backup secrets**: Ensure SOPS secrets are backed up before major updates