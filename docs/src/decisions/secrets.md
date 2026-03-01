# Secrets Management

## Overview

This setup uses [SOPS](https://github.com/mozilla/sops) for secrets management, integrated with NixOS through [sops-nix](https://github.com/Mic92/sops-nix).

## Why SOPS?

SOPS was chosen over alternatives for several reasons:

| Criteria | SOPS | Ansible Vault | HashiCorp Vault |
|----------|------|---------------|-----------------|
| Nix integration | Native | Limited | No native support |
| Encryption backends | age, GPG, KMS, AWS | AES | AES |
| YAML/JSON support | Yes | Yes | Yes |
| Version control friendly | Yes | Yes | No (running service) |
| Complexity | Low | Medium | High |

### Key Advantages

1. **Key-based encryption**: Multiple backends supported (age, GPG, KMS)
2. **YAML/JSON native**: Easy integration with Nix configurations
3. **Version control friendly**: Encrypted secrets can be committed safely
4. **Decryption on demand**: Secrets decrypted at build or runtime
5. **Wide adoption**: Standard in Nix community via sops-nix

## Encryption Key Management

### Age Keys

Age was chosen as the primary encryption backend:

- **Simpler key format**: Single file vs complex GPG key rings
- **Faster key generation**: Instant key creation
- **Easier management**: Single private key file
- **Good enough for personal use**: No need for complex GPG workflows

```
secrets/
├── age.pub              # Public encryption key
├── secrets.age          # Private encryption key (encrypted)
├── secrets.json         # Encrypted secrets file
└── README.md            # Key management instructions
```

## Integration with NixOS

The sops-nix module provides seamless integration:

```nix
{ config, pkgs, inputs, ... }:

{
  # Enable sops-nix
  services.sops = {
    enable = true;
    defaultSopsFile = ../../secrets/secrets.json;
    age.keyFile = ../../secrets/secrets.age;
  };

  # Declare secrets
  secrets.mySecret = {
    sopsFile = ../../secrets/secrets.json;
    neededForBoot = true;
  };
}
```

## Secrets Structure

The secrets file is organized by category:

```yaml
# secrets/secrets.json
{
  "ssh_keys": {
    "encrypted_sops": "age-encrypted-data",
    "sops_unencrypted_metadata": { ... }
  },
  "docker_credentials": {
    "encrypted_sops": "age-encrypted-data",
    "sops_unencrypted_metadata": { ... }
  },
  "api_keys": {
    "encrypted_sops": "age-encrypted-data",
    "sops_unencrypted_metadata": { ... }
  }
}
```

## Access Control

Who can access secrets?

1. **Development environment**: Anyone with the age key can decrypt
2. **Production systems**: SOPS automatically decrypts at boot
3. **CI/CD**: Can be configured with GPG or KMS keys

### Security Considerations

- **Age key storage**: Should be encrypted or stored securely
- **Repository access**: Anyone with repo access can decrypt if they have the key
- **Backup strategy**: Age key backup is critical (equivalent to root access)
- **Rotation**: Can rotate encryption keys without changing content

## Why Not Other Approaches?

### Ansible Vault

**Considered**: Same encryption as SOPS but different workflow.

**Rejected**: Less integrated with Nix ecosystem, imperative vs declarative.

### HashiCorp Vault

**Considered**: Enterprise-grade secrets management.

**Rejected**: Overkill for personal use, requires running service, adds complexity.

### Plain Text with Git Encryption

**Considered**: Encrypt entire repository.

**Rejected**: Coarse-grained, can't encrypt specific values within files.

## Best Practices

### 1. Minimal Secrets

Only secrets that must be stored should be included. Application configuration that doesn't require secrets should be plaintext.

### 2. Regular Rotation

Rotate encryption keys periodically and re-encrypt secrets.

### 3. Backup Keys

The age private key is critical - back it up securely.

### 4. Access Control

Limit who has access to the secrets file and encryption keys.

## Editing Secrets

```bash
# Edit secrets using the menu
menu secrets

# Or directly with sops
sops secrets/secrets.json
```

## Next Steps

- [Deployment strategy](deployment.md) - How secrets are deployed
- [Security considerations](../design/declarative.md) - Overall security model
