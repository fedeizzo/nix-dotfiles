# Disaster Recovery

This document describes the disaster recovery strategy for the homelab, including backup architecture, recovery procedures, and common failure scenarios.

## Recovery Philosophy

The recovery strategy is designed around three principles:

1. **Immutable infrastructure**: Rebuild from configuration rather than patching
2. **Encrypted offsite backups**: Restic to Backblaze B2 for disaster resilience
3. **Local snapshots**: BTRFS for quick rollback of recent changes

### What This Setup Makes Easy

- **Full system rebuild**: `scripts/installation/framework-desktop.sh` recreates the entire system
- **Quick rollback**: BTRFS snapshots allow instant revert to previous state
- **Point-in-time recovery**: Restic snapshots enable recovery from any backed-up point

### What This Setup Makes Harder

- **Ephemeral root**: `/` is erased on boot, requiring careful persistence configuration
- **Database version drift**: PostgreSQL version upgrades can corrupt data
- **Secrets dependency**: SOPS keys must be available for restoration

## Backup Architecture

### Local Backups: BTRFS Snapshots

- **Location**: `/persist` (BTRFS subvolume)
- **Mechanism**: Automatic snapshots on every boot
- **Retention**: Configured in `impermanence` module
- **Recovery time**: Seconds (mount previous snapshot)

### Offsite Backups: Restic + Backblaze B2

- **Location**: Backblaze B2 cloud storage
- **Mechanism**: Encrypted incremental backups via restic
- **Schedule**: Daily automatic backups
- **Retention**: Configurable via restic policy
- **Recovery time**: Hours (depends on data size and network)

### Backup Coverage

| Data | Local Snapshots | Restic Backup |
|------|-----------------|---------------|
| `/persist` | Yes | Yes |
| `/games` | Optional | Optional |
| `/home/media` | Optional | Optional |
| Config (git) | N/A | N/A (git push) |

## Recovery Workflow

### Scenario 1: Single Service Failure

**Example**: PostgreSQL crashes, database corruption detected

**Recovery**:
1. Check available PostgreSQL dumps: `ls /persist/var/backup/postgresql/`
2. Stop the service: `systemctl stop postgresql`
3. Restore from dump or repair database
4. Restart service: `systemctl start postgresql`

See "Service-Specific Recovery" below for detailed procedures.

### Scenario 2: System Corruption

**Example**: Multiple services failing, configuration drift

**Recovery**:
1. Boot into BTRFS snapshot from previous day
2. Verify services start correctly
3. If successful, continue; if not, try earlier snapshot
4. Rebuild configuration if needed: `nixos-rebuild switch`

### Scenario 3: Complete Hardware Failure

**Example**: Disk failure, need to rebuild on new hardware

**Recovery**:
1. Install NixOS via ISO on new hardware
2. Run installation script: `sudo scripts/installation/framework-desktop.sh`
3. Script will:
   - Fetch SOPS keys from Bitwarden
   - Fetch restic credentials
   - Partition disk with BTRFS
   - Restore `/persist` from Backblaze backup
   - Optionally restore `/games` and `/home/media` from external SSD
4. Reboot and verify services

See "Pre-Migration Steps" below for pre-shutdown procedures.

## Pre-Migration Steps

Before shutting down the old system (planned migration or maintenance):

### 1. Create Final Backup

```bash
# Trigger immediate backup
systemctl start restic-backups-backblaze.service

# Wait for completion
systemctl status restic-backups-backblaze.service

# Verify backup succeeded
restic-backblaze snapshots | tail -20
```

### 2. Stop Critical Services

Ensures consistent state for databases:

```bash
# Stop services that write to databases
systemctl stop immich-server immich-machine-learning
systemctl stop paperless-web paperless-scheduler paperless-task-queue
systemctl stop home-assistant
systemctl stop nextcloud-setup

# Trigger final PostgreSQL dump
systemctl start postgresqlBackup.service
systemctl status postgresqlBackup.service

# Wait a few minutes, then trigger final backup
systemctl start restic-backups-backblaze.service
```

### 3. Backup Media Directories to External SSD

```bash
# Prepare external drive (identify with lsblk)
lsblk

# Mount external drive (example: /dev/sda1)
mkdir -p /mnt/backup_ssd
mount /dev/sda1 /mnt/backup_ssd

# Backup /games (Jellyfin media library + Deluge data)
rsync -avhP --delete /games/ /mnt/backup_ssd/games_backup/

# Backup /home/media (Deluge downloads/cache)
rsync -avhP --delete /home/media/ /mnt/backup_ssd/home_media_backup/

# Verify backup
ls -lh /mnt/backup_ssd/

# Unmount
umount /mnt/backup_ssd
```

### 4. Verify Backup Integrity

```bash
# Check latest snapshot
restic-backblaze snapshots | tail -5

# Verify critical files are in backup
restic-backblaze ls latest | grep -E "acme.json|postgresql|zigbee2mqtt/configuration.yaml"

# Optional: Test restore of small critical file
restic-backblaze restore latest --target /tmp/test-restore \
  --include /persist/var/volumes/traefik/acme.json
ls -lh /tmp/test-restore/persist/var/volumes/traefik/acme.json
rm -rf /tmp/test-restore
```

### 5. Document Current State

```bash
# Export service status
systemctl list-units --state=running > /tmp/running-services.txt

# Export disk usage
df -h > /tmp/disk-usage.txt

# Export network config (if needed for new hardware)
ip addr > /tmp/network-config.txt
```

### 6. Shutdown Safely

```bash
# Final sync
sync

# Shutdown
shutdown -h now
```

## Service-Specific Recovery

### PostgreSQL Database Corruption

**Symptoms**:
- PostgreSQL fails to start
- Error: `data directory is of wrong version`
- Error: `checksum mismatch` or `page verification failed`

**Recovery**:

```bash
# 1. Check if dumps exist
ls -lh /persist/var/backup/postgresql/

# 2. Stop PostgreSQL
sudo systemctl stop postgresql

# 3. Backup corrupted data (optional)
sudo mv /persist/var/lib/postgresql /persist/var/lib/postgresql.corrupted

# 4. Rebuild as postgres user
sudo -u postgres initdb -D /persist/var/lib/postgresql --data-checksums

# 5. Start PostgreSQL
sudo systemctl start postgresql

# 6. Restore each database from dumps
cd /persist/var/backup/postgresql
for db in nextcloud paperless immich authentik affine; do
  sudo -u postgres createdb $db
  sudo -u postgres pg_restore -d $db ${db}.dump
done

# 7. Restart dependent services
sudo systemctl restart nextcloud-setup
sudo systemctl restart paperless-web
sudo systemctl restart immich-server
```

**Data loss implications**:
- **Nextcloud**: Files intact, lose shares/calendar/contacts
- **Paperless**: Documents intact, lose tags/metadata
- **Immich**: Photos intact, lose albums/faces/metadata
- **Authentik**: Reconfigure SSO from scratch
- **Affine**: Workspace data lost

### SQLite Database Corruption

#### Grafana

**Symptoms**:
- Grafana fails to start
- Error: `database disk image is malformed`

**Recovery**:

```bash
# 1. Try SQLite repair
cd /persist/var/volumes/grafana/data
sqlite3 grafana.db ".recover" | sqlite3 grafana_recovered.db
mv grafana.db grafana.db.corrupted
mv grafana_recovered.db grafana.db

# 2. If repair fails, start fresh
mv grafana.db grafana.db.corrupted
systemctl restart grafana

# 3. Reconfigure
# - Re-add data sources (Prometheus, Loki, InfluxDB)
# - Re-import dashboards (if not provisioned)
# - Recreate users/org settings
```

#### Subtrackr

**Symptoms**:
- Service fails to start
- Error: `database is locked` or `malformed`

**Recovery**:

```bash
# Delete and restart (subscription data lost)
rm /persist/var/lib/subtrackr/subtrackr.db
systemctl restart subtrackr

# Manual re-entry of subscriptions required
```

### Home Assistant

**Symptoms**:
- HA fails to start
- Error: `Database malformed` or `cannot open database`

**Recovery**:

```bash
# 1. Check database
cd /persist/var/lib/hass
sqlite3 home-assistant_v2.db "PRAGMA integrity_check;"

# 2. If corrupted, delete history (config survives)
mv home-assistant_v2.db home-assistant_v2.db.corrupted
systemctl restart home-assistant

# 3. Verify
# - Automations intact (YAML config)
# - Device states reload from integrations
# - History lost (regenerates from this point)
```

### Zigbee2MQTT Network Key Lost

**Symptoms**:
- Zigbee devices not responding
- `/persist/var/lib/zigbee2mqtt/configuration.yaml` missing or corrupted

**Recovery**:

**CRITICAL**: If network key is lost, ALL devices must be re-paired.

```bash
# Check if configuration exists
cat /persist/var/lib/zigbee2mqtt/configuration.yaml

# If lost, devices are irrecoverable without manual re-pairing:
# 1. Factory reset each Zigbee device (device-specific)
# 2. Zigbee2MQTT will generate new network key
# 3. Pair each device one by one via Z2M UI
```

**Prevention**: The backup includes this file. If restore succeeded, you're safe.

### Traefik SSL Certificates Lost

**Symptoms**:
- HTTPS sites show certificate errors
- `/persist/var/volumes/traefik/acme.json` missing or empty

**Recovery**:

```bash
# 1. Check if file exists and has content
ls -lh /persist/var/volumes/traefik/acme.json

# 2. If missing, Traefik will request new certs
# Delete corrupted file
rm /persist/var/volumes/traefik/acme.json

# 3. Restart Traefik (will trigger ACME challenge)
systemctl restart traefik

# 4. Monitor logs
journalctl -u traefik -f

# Certificates will be reissued (may take 5-10 minutes)
# Rate limit: 50 certs/week, so don't do this repeatedly
```

### InfluxDB Data Corruption

**Symptoms**:
- InfluxDB fails to start
- Error: `unable to open wal segment` or `snapshot corrupt`

**Recovery**:

```bash
# 1. Check InfluxDB logs
journalctl -u influxdb -n 100

# 2. Try repair
sudo systemctl stop influxdb
cd /persist/var/lib/influxdb
sudo -u influxdb influxd inspect verify-wal

# 3. If repair fails, delete WAL and restart
rm -rf /persist/var/lib/influxdb/wal/*
sudo systemctl start influxdb

# Data loss: Recent metrics (last few minutes to hours)
```

### Immich Database Mismatch

**Symptoms**:
- Immich web UI shows errors
- Error: `database schema version mismatch`

**Recovery**:

```bash
# Immich includes auto-migration
# 1. Stop immich services
systemctl stop immich-server immich-machine-learning

# 2. Backup database
sudo -u postgres pg_dump immich > /tmp/immich_backup.sql

# 3. Restart services (will auto-migrate)
systemctl start immich-server immich-machine-learning

# 4. If migration fails, check logs
journalctl -u immich-server -f

# Manual intervention rarely needed
```

### Nextcloud Stuck in Maintenance Mode

**Symptoms**:
- Nextcloud shows "Maintenance mode" page
- Database restored but maintenance flag stuck

**Recovery**:

```bash
# Disable maintenance mode
sudo -u nextcloud nextcloud-occ maintenance:mode --off

# Repair if needed
sudo -u nextcloud nextcloud-occ maintenance:repair

# Clear cache
sudo -u nextcloud nextcloud-occ files:scan --all
```

### Service-Specific File Permissions

**Symptoms**:
- Service fails to start
- Error: `Permission denied` reading config/database

**Recovery**:

```bash
# PostgreSQL
chown -R postgres:postgres /persist/var/lib/postgresql
chmod 0750 /persist/var/lib/postgresql

# Immich
chown -R immich:immich /persist/var/lib/immich

# Home Assistant
chown -R hass:hass /persist/var/lib/hass

# Zigbee2MQTT
chown -R zigbee2mqtt:zigbee2mqtt /persist/var/lib/zigbee2mqtt

# Media user (Deluge, Jellyfin, etc.)
chown -R 800:1800 /home/media
chown -R media:media /games/jellyfin

# Apply all at once
nixos-rebuild switch
```

## Verification Commands

After recovery, verify system health:

```bash
# Check all services
systemctl list-units --failed

# Database connectivity
sudo -u postgres psql -c "SELECT version();"
sudo -u postgres psql -l

# Storage space
df -h /persist /games /home/media

# Recent logs
journalctl -p err -b

# Backup status
systemctl status restic-backups-backblaze.service
```

## When to Restore from Earlier Backup

If multiple critical services are corrupted:

```bash
# 1. List available snapshots
restic-backblaze snapshots

# 2. Choose an older snapshot
restic-backblaze restore <snapshot-id> --target /persist

# 3. Reboot
reboot
```

## Prevention: Regular Testing

```bash
# Monthly: Verify PostgreSQL dumps are valid
pg_restore --list /persist/var/backup/postgresql/nextcloud.dump

# Quarterly: Test restore in VM
# - Restore /persist from backup
# - Boot system
# - Verify all services start

# After major changes: Create manual backup
restic-backblaze backup /persist
```

## Emergency Contacts / Resources

- **PostgreSQL recovery**: https://www.postgresql.org/docs/current/backup-dump.html
- **Restic manual restore**: https://restic.readthedocs.io/en/latest/050_restore.html
- **NixOS manual**: https://nixos.org/manual/nixos/stable/

## Recovery Time Objectives

| Scenario | RTO (Recovery Time Objective) | RPO (Recovery Point Objective) |
|----------|------------------------------|-------------------------------|
| Single service failure | 30 minutes | Last backup |
| System corruption | 2 hours | Last boot snapshot |
| Complete hardware failure | 4-8 hours | Last restic backup |

**Note**: RTO/RPO depend on data size, network speed, and backup freshness.