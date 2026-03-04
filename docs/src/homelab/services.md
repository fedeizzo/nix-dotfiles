# Services Overview

This document describes the homelab services architecture, organization, and the custom module system used to manage them.

## Custom Service Module

Instead of using individual NixOS service modules, a custom `fi.services` module provides a unified interface for service management. This approach was chosen for:

- **Centralized configuration**: All service metadata (ports, persistence, backup) in one place
- **Consistent patterns**: Every service follows the same structure
- **Automated persistence**: `toPersist` automatically configures BTRFS snapshot preservation
- **Automated backup**: `toBackup` automatically configures restic backup paths
- **Dashboard organization**: `dashboardSection` groups services in the proxy dashboard

### Service Schema

```nix
{
  name = "service-name";           # Unique identifier
  subdomain = "service";           # DNS subdomain (defaults to name)
  port = 8080;                     # Internal port
  path = "";                       # URL path (default: root)
  isExposed = true;                # Expose to internet via traefik
  authType = "proxy";              # Authentication: "proxy" (authentik) or "none"
  dashboardSection = "Media";      # Dashboard grouping
  dashboardIcon = "service";       # Icon name for dashboard
  toPersist = [ ... ];             # Directories to preserve across reboots
  toBackup = [ ... ];              # Paths included in restic backups
}
```

## Service Categories

### Exposed Services

Services directly accessible from the internet, typically personal projects.

| Service | Subdomain | Port | Purpose |
|---------|-----------|------|---------|
| fedeizzodev | fedeizzo.dev | 50001 | Personal Hugo website |

### Exposed Media Services

Media services exposed to the internet, protected by authentik authentication via traefik proxy.

| Service | Subdomain | Port | Purpose |
|---------|-----------|------|---------|
| jellyfin | photo.fedeizzo.dev | 8096 | Media server (movies, TV) |
| jellyseerr | - | 5055 | Request management for Jellyfin |
| nextcloud | nextcloud.fedeizzo.dev | 8180 | Cloud storage and collaboration |
| collabora | nextcloud.fedeizzo.dev | 9980 | Online office suite (Nextcloud integration) |
| affine | affine.fedeizzo.dev | 3010 | Note-taking and project management |

### Media Services

Media management and streaming services (internal only).

| Service | Port | Purpose |
|---------|------|---------|
| immich | 2283 | Photo and video management (Google Photos alternative) |
| paperless | 8000 | Document management system |
| fusion | 80 | TinyTinyRSS feed reader |
| sonarr | 8989 | TV show management and automation |
| radarr | 7878 | Movie management and automation |
| prowlarr | 9696 | Indexer management for Sonarr/Radarr |
| deluge | 8112 | BitTorrent client |
| bazarr | 6767 | Movie and TV show subtitle management |
| calibre | 8083 | E-book library management |

### Tools

Utility and productivity services.

| Service | Port | Purpose |
|---------|------|---------|
| glance | 80 | Dashboard aggregator (displays multiple services) |
| uptime-kuma | 3001 | Service monitoring and status page |
| net-worth | 3000 | Personal finance tracking |
| climbing-lab | 3000 | Climbing gym tracking and statistics |

### Monitoring Stack

Observability services for system health monitoring.

| Service | Port | Purpose |
|---------|------|---------|
| prometheus | 9090 | Metrics collection and alerting |
| grafana | 3000 | Metrics visualization and dashboards |
| loki | 3100 | Log aggregation (Prometheus ecosystem) |
| influxdb | 8086 | Time-series metrics database |

### Infrastructure

Core infrastructure services that other services depend on.

| Service | Port | Purpose |
|---------|------|---------|
| authentik | 9000 | Identity provider and SSO (authentication for all services) |
| traefik | 80, 443 | Reverse proxy and ACME certificate management |
| blocky | 4000 | DNS server and ad-blocker |
| postgres | 5432 | Central PostgreSQL database (used by multiple services) |

### Home Automation

Smart home and IoT integration.

| Service | Port | Purpose |
|---------|------|---------|
| home-assistant | 8123 | Home automation platform |
| zigbee2mqtt | 8883 | Zigbee to MQTT bridge |

### AI/LLM Services

Local AI and machine learning services.

| Service | Port | Purpose |
|---------|------|---------|
| ollama | 11434 | Local LLM inference server |
| open-webui | 8080 | Web interface for Ollama |
| llama-swap | 8081 | LLM model switching and management |

### Communication

| Service | Port | Purpose |
|---------|------|---------|
| opencloud | 443 | Nextcloud-based collaboration suite (commented out) |

### Utilities

| Service | Port | Purpose |
|---------|------|---------|
| logrotate | - | Log rotation configuration |
| streaming | - | Streaming services configuration |

## Persistence Strategy

Services with `toPersist` have their data directories preserved across reboots via BTRFS snapshots. This is critical because:

- `/` is ephemeral (erased on boot)
- Only `/persist` survives reboots
- The `fi.services` module automatically creates the necessary configuration

Example persistence configuration:

```nix
toPersist = [
  {
    directory = config.services.immich.mediaLocation;
    user = "immich";
    group = "immich";
    mode = "u=rwx,g=rx,o=";
  }
];
```

This creates:
- BTRFS subvolume at the specified path
- Mount configuration in `/etc/fstab`
- Directory with correct ownership and permissions

## Backup Strategy

Services with `toBackup` have their paths included in restic backups to Backblaze B2. The backup configuration:

- Runs periodically via systemd timer
- Encrypts data with SOPS-managed keys
- Retains multiple snapshots for recovery
- Includes both `/persist` and specific service data paths

Example backup configuration:

```nix
toBackup = [
  "/persist${config.services.immich.mediaLocation}/backups"
  "/persist${config.services.immich.mediaLocation}/library"
  "/persist${config.services.immich.mediaLocation}/upload"
  "/persist${config.services.immich.mediaLocation}/profile"
];
```

## Authentication Flow

Services with `authType = "proxy"` are protected by authentik via traefik:

1. User accesses `service.fedeizzo.dev`
2. Traefik redirects to authentik for authentication
3. User logs in (supports MFA, SSO, etc.)
4. Authentik redirects back with JWT token
5. Traefik validates token and forwards request
6. Service receives authenticated request

Services with `authType = "none"` are publicly accessible (not recommended for sensitive data).

## Service Dependencies

### Database Dependencies

Multiple services share a single PostgreSQL instance:

| Service | Database |
|---------|----------|
| nextcloud | nextcloud |
| paperless | paperless |
| immich | immich |
| authentik | authentik |
| affine | affine + redis-affine |

This reduces resource usage but creates a single point of failure.

### Commented-Out Services

Some services are commented out in the imports:

- `opencloud`: Nextcloud fork with improved performance (not yet tested)
- `sunshine`: Game streaming service (not currently needed)

## Adding New Services

To add a new service:

1. Create service directory under `hosts/framework-desktop/services/`
2. Define the service configuration in Nix
3. Add to `imports` in `default.nix`
4. Add to `fi.services` array with metadata
5. Configure persistence and backup if needed
6. Test locally before deployment

See `development/adding-services.md` for detailed instructions.

## Resource Usage

Typical resource consumption (Raspberry Pi 4 / Framework Desktop):

- **CPU**: 2-4 cores idle, spikes to 8 during builds/LLM inference
- **RAM**: 8-12GB used (PostgreSQL + Immich + Home Assistant are largest consumers)
- **Storage**: 500GB+ for `/persist`, 2TB+ for media in `/games` and `/home/media`

## Monitoring

Service health is monitored via:

- **Uptime Kuma**: External availability monitoring
- **Prometheus**: Resource metrics (CPU, RAM, disk)
- **Grafana**: Dashboards for visualization
- **Loki**: Log aggregation for debugging

Access monitoring stack via `grafana.fedeizzo.dev` (authenticated).