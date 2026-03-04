# Secrets Management

This guide covers managing encrypted secrets using SOPS.

## Overview

Secrets are encrypted using SOPS with age encryption. Each host has its own secrets file.

**Secrets location:**
- `hosts/<host>/system/<secret-file>.yaml.enc`
- `hosts/<host>/home/<secret-file>.yaml.enc`

**Encryption:**
- Algorithm: age
- Keys: Stored in password manager (Bitwarden/rbw)
- Files: Encrypted in place (`.yaml.enc` extension)

## Editing Secrets

### Using the Edit Script (Recommended)

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

### Manual Editing

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

## Adding New Secrets

### 1. Add to SOPS Configuration

Edit `.sops.yaml` to define encryption keys:
```yaml
creation_rules:
  - path_regex: .*\.yaml\.enc$
    age: "age-1<key1>,age-1<key2>"
```

### 2. Add Secret to Host File

```yaml
# hosts/<host>/system/oven-secrets.yaml.enc (after decrypting)
my_secret: "secret-value"
database_password: "db-password"
api_key: "api-key-value"
```

### 3. Reference Secret in Configuration

```nix
# In configuration.nix or service module
sops.secrets.my_secret = {
  source = "my_secret";
};

# Use in service configuration
environment.variables.MY_SECRET = config.secrets.my_secret.path;
```

## Secrets in Service Modules

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

## Managing Age Keys

### Viewing Age Keys

```bash
# View public key
cat hosts/<host>/system/age-key.txt.pub

# View private key (careful!)
cat hosts/<host>/system/age-key.txt
```

### Adding New Key Recipient

```bash
# Generate new age key pair
age-keygen -o age-key.txt

# Get public key
cat age-key.txt.pub

# Update .sops.yaml with new public key
# Add to password manager
```

### Rotating Secrets

```bash
# Decrypt secrets
sops decrypt hosts/<host>/system/oven-secrets.yaml.enc > /tmp/secrets.yaml

# Re-encrypt with new keys
sops --encrypt --age "age-1<old-key>,age-1<new-key>" /tmp/secrets.yaml > hosts/<host>/system/oven-secrets.yaml.enc

# Remove old key after verification
# Update .sops.yaml
```

## Common Secrets Issues

| Issue | Solution |
|-------|----------|
| "Permission denied" when accessing secrets | Ensure secret file has correct permissions (600) |
| | Check that sops-nix is properly configured |
| "Key not found" error | Verify age key is in your key agent |
| | Check that the key matches the recipient in .sops.yaml |
| Secrets not appearing in environment | Ensure secrets are properly referenced in configuration |
| | Check that the service is restarted after secret changes |

## Best Practices

1. **Never commit plaintext secrets**: Always keep secrets encrypted
2. **Use separate keys per host**: Different hosts should have different age keys
3. **Rotate keys regularly**: Update age keys every 6-12 months
4. **Backup keys securely**: Store age private keys in password manager
5. **Test secret changes**: Verify secrets are accessible after changes
6. **Document secret usage**: Add comments explaining what each secret is for

## SOPS Configuration

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

## Editing Workflow

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