# Framework Desktop (Homelab Server)

## Overview

The **Framework Desktop** serves as the primary homelab server, running a full NixOS system with various services. This machine represents the "server-first" approach where the focus is on reliability, security, and service availability rather than desktop experience.

**Hostname**: `homelab`  
**Architecture**: x86_64  
**OS**: NixOS with full service deployment  
**Status**: Active primary homelab

## Hardware Characteristics

While specific hardware details depend on the exact Framework Desktop configuration, the homelab is designed around:

- **x86_64 architecture**: Full Linux compatibility and performance
- **Server-grade hardware**: Reliable, consistent hardware that can run 24/7
- **Network connectivity**: Always-on network access for remote management
- **Storage**: Configured for BTRFS snapshots with impermanence

## Configuration Approach

### System Type

The homelab is a **server-only** system with no desktop environment:

- Pure command-line interface
- SSH-only access for administration
- Focus on running services rather than user applications
- Optimized for remote management and automation

### Services Running

The homelab hosts several key services that form the backbone of the home infrastructure:

#### Authentik

**Purpose**: Identity and access management service

```nix
# Authentication and access control for internal services
services.authentik = {
  enable = true;
  settings = {
    # Configuration for identity provider
    # Single sign-on for all internal services
  };
};
```

**Role**:
- Centralized authentication for all homelab services
- User account management
- Single sign-on (SSO) capability
- Integration with external authentication providers

#### Climbing Lab

**Purpose**: Personal tracking application

```nix
# Climbing lab tracking application
climbing-lab = {
  enable = true;
  # Custom configuration
};
```

**Role**:
- Personal data tracking
- Activity logging
- Personal analytics

### Deployment Strategy

The homelab uses **remote deployment** with automatic rollback capabilities:

```nix
# deployment.nix configuration
deploy.nodes.homelab = {
  hostname = "homelab";
  sshUser = "root";
  sudo = "doas -u";
  magicRollback = true;
  autoRollback = true;
  remoteBuild = true;
};
```

**Key Features**:

1. **Remote Builds**: The configuration is built on the developer's machine and deployed to the homelab, reducing build time on the server
2. **Magic Rollback**: If deployment fails, the system automatically rolls back to the previous working state
3. **SSH Deployment**: Secure, automated deployment over SSH
4. **No Local Builds**: The homelab doesn't need to run the Nix build process, saving resources

### Security Considerations

The homelab implements several security measures:

#### Access Control

- **SSH-only access**: No direct console access required
- **Root access via SSH**: Simplified management but requires careful key management
- **Secret encryption**: All sensitive configuration encrypted with SOPS

#### Network Security

- **Firewall**: Configured to allow only necessary services
- **Port exposure**: Minimal ports exposed to the internet
- **Service isolation**: Each service runs in its own container or VM where possible

#### Backup Strategy

- **Stateless operation**: Most state is ephemeral
- **Persistent data**: Critical data is backed up separately
- **Rebuild capability**: System can be rebuilt from scratch if needed

## Service Architecture

### Service Relationships

```
┌─────────────────┐
│  Authentik      │
│  (Identity)     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│ Climbing Lab    │────▶│ Authentik       │
└─────────────────┘     │ (Authentication)│
                        └─────────────────┘
```

### Service Dependencies

- **Authentik**: Core authentication service, minimal dependencies
- **Climbing Lab**: Depends on Authentik for authentication
- **System services**: NixOS base services (SSH, networking, etc.)

## Maintenance and Monitoring

### Automated Updates

- **Configuration updates**: Can be triggered via `deploy` command
- **Automatic rollback**: Failed deployments automatically revert
- **Version control**: All changes tracked in git repository

### Monitoring Considerations

- **Service health**: Services can be monitored via health checks
- **Resource usage**: System resources are available for monitoring tools
- **Log management**: Logs stored in `/var/log`, can be monitored remotely

## Why This Approach?

The homelab setup follows several design principles:

### 1. Service-First

The focus is on running services reliably rather than providing a desktop experience. This means:
- No unnecessary packages or services
- Optimized for uptime and reliability
- Minimal attack surface

### 2. Automated Management

Everything is automated:
- Deployments are automated via `deploy-rs`
- Rollbacks happen automatically on failure
- No manual intervention required for routine tasks

### 3. Declarative Everything

The entire system state is declared in Nix:
- Services and their configurations
- Network settings
- Security policies
- User accounts and permissions

### 4. Security by Design

Security is built into the architecture:
- Encryption at rest (SOPS)
- Secure deployment (SSH with keys)
- Minimal exposure (firewall configuration)
- Automatic rollback on failures

## Comparison to Other Systems

### vs. Personal Laptop (X1 Nano)

| Aspect | Homelab | X1 Nano |
|--------|---------|---------|
| Interface | SSH only | Desktop environment |
| Focus | Services | User experience |
| Updates | Automated rollback | Manual control |
| Security | Server-focused | Personal-focused |
| Persistence | Selective | User-driven |

### vs. Work Machine (MacBook Pro)

| Aspect | Homelab | MacBook Pro |
|--------|---------|-------------|
| Control | Full | Limited |
| OS | NixOS | macOS + Nix |
| Customization | Complete | Partial |
| Use case | Server | Desktop |

## Future Considerations

Potential improvements or additions:

- **Containerization**: More services in containers for isolation
- **Monitoring**: Integrated monitoring and alerting
- **Redundancy**: Additional backup homelab
- **Automated testing**: Pre-deployment validation
- **Performance monitoring**: Resource usage tracking

## Next Steps

Understanding the homelab setup provides context for:
- [Network topology](../homelab/network.md)
- [Deployment process](../decisions/deployment.md)
- [Secrets management](../decisions/secrets.md)

The homelab represents the server-side of the infrastructure, complementing the personal and work machines in the overall setup.
