# BTRFS Snapshots & Impermanence

## The Erase-on-Boot Philosophy

One of the most distinctive features of this setup is the **erase-and-rebuild-on-every-boot** approach for personal machines. This radical approach to system statelessness provides unique guarantees that traditional setups cannot match.

## How It Works

### The Boot Process

```
┌─────────────────────────────────────────────────────────────┐
│                    Boot Sequence                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Machine powers on                                       │
│     └─ UEFI firmware initializes                            │
│                                                             │
│  2. Bootloader (systemd-boot / GRUB) loads                  │
│     └─ Shows available system generations                   │
│                                                          │
│  3. Kernel boots from selected generation                   │
│     └─ NixOS bootloader creates BTRFS snapshot              │
│                                                             │
│  4. Initrd runs                                             │
│     └─ Mounts BTRFS filesystem                              │
│     └─ Runs impermanence scripts                            │
│                                                             │
│  5. Root filesystem mounted                                 │
│     └─ From BTRFS snapshot                                  │
│                                                             │
│  6. Impermanence restores persistent directories            │
│     └─ /home → from persistent storage                      │
│     └─ /var → from persistent storage                       │
│                                                             │
│  7. System ready                                            │
│     └─ Fresh state, user data restored                      │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### BTRFS Subvolumes

The system uses BTRFS subvolumes to manage different filesystem areas:

```
/
├── @/                          (root subvolume)
│   ├── boot/                   (boot files)
│   └── nix/                    (Nix store)
├── @home/                      (home subvolume)
│   └── user/                   (user home directory)
└── @var/                       (var subvolume)
    ├── log/                    (logs)
    └── cache/                  (caches)
```

Each subvolume can have its own mount options and characteristics.

### Impermanence Configuration

The [impermanence](https://github.com/nix-community/impermanence) NixOS module handles the stateless setup:

```nix
{ config, pkgs, lib, ... }:

{
  # Enable impermanence
  services.impermanence = {
    enable = true;
    
    # Define which filesystems to persist
    filesystems = {
      "/".neededForBoot = true;
      "/home" = {};
      "/var" = {};
    };
    
    # Directories that should be empty at boot
    emptyFilesystems = [
      "/var/log"
      "/var/tmp"
      "/tmp"
    ];
    
    # Bind mounts for persistent directories
    postBootCommands = ''
      # Create directories if they don't exist
      mkdir -p /home/user/.cache
      mkdir -p /var/cache
    '';
  };
  
  # Configure BTRFS
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/...";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "noatime"
      "subvol=root"
    ];
  };
  
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/...";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "noatime"
      "subvol=@home"
    ];
  };
  
  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/...";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "noatime"
      "subvol=@var"
    ];
  };
  
  # Enable automatic BTRFS scrubbing
  services.btrfs.autoScrub.enable = true;
  
  # Configure snapshot management
  boot.initrd.btrfs.devices = {
    "/dev/disk/by-uuid/..." = {};
  };
}
```

## Data Persistence Strategy

### What Persists Across Boots

The following directories are preserved through the BTRFS snapshots and bind mounts:

#### `/home/*` (User Data)

- **User dotfiles**: All configuration files
- **Personal files**: Documents, downloads, etc.
- **Development projects**: Source code, projects
- **Email data**: Local email storage
- **Application data**: User-specific application state
- **Cache data**: Application caches

**Rationale**: User data should never be lost. Personal files and configuration are the core of the user's workflow.

#### `/var/cache/*` (Cached Data)

- **Package caches**: Apt, pip, npm, etc.
- **Application caches**: Browser caches, media caches
- **Build caches**: Compiler, build system caches

**Rationale**: Caches can be regenerated but improve performance. Preserving them speeds up operations while still allowing refresh.

#### `/var/lib/*` (Persistent Runtime Data)

- **Database files**: PostgreSQL, MySQL data
- **Service state**: Docker containers, systemd state
- **Application state**: Application-specific persistent data

**Rationale**: Application state needs to persist between reboots.

### What Does Not Persist

The following directories are recreated fresh on every boot:

#### `/var/log/*` (Logs)

- **System logs**: syslog, journal
- **Application logs**: Service logs
- **User logs**: Shell history, application logs

**Rationale**: Logs are ephemeral by nature. They can be regenerated and are not critical for system operation.

#### `/var/tmp/*` (Temporary Files)

- **Temporary files**: Application temporary files
- **Build artifacts**: Intermediate build files
- **Cache files**: Short-term cache data

**Rationale**: Temporary files should not persist. They can be regenerated if needed.

#### `/tmp/*` (System Temporary Files)

- **System temporary files**: System-wide temporary data
- **Lock files**: Process locks
- **Runtime data**: Runtime-specific temporary data

**Rationale**: System temporary files are ephemeral and should not persist.

## Benefits of Erase-on-Boot

### 1. Guaranteed Consistency

Every boot starts from a known, declared state:

- **No configuration drift**: Nothing accumulates outside the configuration
- **Predictable behavior**: System behaves exactly as declared
- **Reproducible**: Same configuration, same system state

### 2. Simplified Debugging

When something breaks, it's easy to fix:

- **Fresh start**: Next boot clears the issue
- **Known state**: System is always in declared state
- **Isolation**: Problems don't persist across boots

### 3. Security

Security through statelessness:

- **Wiped compromise**: Any compromise is reset on next boot
- **No persistent malware**: Malware cannot survive a reboot
- **Fresh state**: No accumulated security issues

### 4. Simplicity

No manual maintenance required:

- **No cleanup needed**: State resets automatically
- **No backup strategy**: System state is declared, not backed up
- **No version control for data**: Data is persistent, configuration is versioned

### 5. Atomicity

All changes are atomic:

- **Fresh state**: Every boot is a fresh start
- **No partial updates**: System is either in declared state or not
- **Rollback**: Previous states are preserved and bootable

## Trade-offs and Considerations

### Performance Impact

Every boot requires:

- **Filesystem recreation**: Directories must be recreated
- **Mount operations**: BTRFS subvolumes must be mounted
- **Bind mounts**: Persistent directories must be bound

**Mitigation**: Modern hardware makes this negligible (seconds).

### Data Management

You must be intentional about:

- **What persists**: Only essential data should persist
- **What doesn't**: Temporary data should not persist
- **Backup strategy**: Important data needs separate backup

**Mitigation**: Clear documentation of what persists and why.

### Not for Everyone

This approach:

- **Is experimental**: Not the standard NixOS path
- **Requires understanding**: Need to understand the philosophy
- **May not fit**: Not suitable for all use cases

**Mitigation**: Document the philosophy clearly, allow users to choose.

## Implementation Details

### Initrd Configuration

The initrd (initial ram filesystem) is responsible for setting up the stateless environment:

```nix
# /etc/nixos/configuration.nix
{ config, pkgs, ... }:

{
  # BTRFS snapshot configuration
  boot.initrd.availableKernelModules = [ "btrfs" ];
  
  # Load BTRFS module early
  boot.initrd.kernelModules = [ "btrfs" ];
  
  # Enable BTRFS snapshot on boot
  boot.initrd.btrfs.devices = {
    "/dev/disk/by-uuid/..." = {};
  };
  
  # Configure BTRFS
  boot.kernel.sysctl = {
    "vm.vfs_cache_pressure" = 50;
  };
  
  # Enable automatic BTRFS scrubbing
  services.btrfs.autoScrub.enable = true;
  services.btrfs.autoScrub.interval = "weekly";
}
```

### Snapshots and Rollback

The system uses BTRFS snapshots for rollback capability:

```nix
# Configure snapshot creation and retention
{ config, lib, pkgs, ... }:

{
  # Enable automatic snapshot creation
  boot.initrd.systemd.enable = true;
  
  # Configure snapshot retention
  boot.initrd.btrfs.subvolumes = {
    "/@home" = {
      subvol = "@home";
      readOnly = false;
    };
    "/@var" = {
      subvol = "@var";
      readOnly = false;
    };
  };
  
  # Allow rollback via NixOS
  boot.supportedFilesystems = [ "btrfs" ];
  boot.supportedFilesystems = [ "btrfs" ];
}
```

### Network Considerations

Network file systems can be mounted during boot:

```nix
# Network-mounted persistent storage
{ config, lib, pkgs, ... }:

{
  # Network storage for persistence
  services.networkfs.enable = true;
  
  # Bind to network storage
  fileSystems."/home" = {
    device = "//server/share";
    fsType = "cifs";
    options = [ "credentials=/etc/cifs-credentials" ];
  };
}
```

## Best Practices

### 1. Document What Persists

Clearly document:

- **What persists**: Which directories are preserved
- **Why they persist**: Rationale for persistence
- **What doesn't persist**: What is recreated on boot

### 2. Regular Backups

Despite statelessness:

- **Important data**: Personal files need backup
- **Configuration**: Already in git repository
- **Critical data**: Databases and important files need backup

### 3. Performance Monitoring

Monitor:

- **Boot time**: Should be reasonable
- **Mount operations**: Should be fast
- **BTRFS health**: Regular scrubbing

### 4. Testing

Test:

- **Boot process**: Ensure it works reliably
- **Persistence**: Verify data persists correctly
- **Rollback**: Ensure rollback works as expected

## Conclusion

The erase-on-boot approach provides unique benefits:

- **Guaranteed consistency** through stateless operation
- **Simplified maintenance** with automatic state reset
- **Security** through statelessness and rollback capability

While it requires careful consideration of what data persists, the long-term benefits for maintenance, security, and reliability make it an attractive option for personal systems where complete control over the infrastructure is desired.

## Next Steps

- [Flakes structure](flakes-structure.md) - How this fits into the overall Nix configuration
- [Deployment strategy](../decisions/deployment.md) - How changes are deployed to systems with this setup
- [Getting started](../tutorial/installation.md) - How to implement this approach yourself
