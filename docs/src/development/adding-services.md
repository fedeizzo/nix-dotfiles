# Adding Services

This guide covers how to add new services to your NixOS dotfiles configuration, both for NixOS (homelab) and Home Manager (personal machines).

## NixOS Services (Framework Desktop/Homelab)

The Framework Desktop uses a custom service module system in `hosts/framework-desktop/services/`. This provides consistent service setup with automatic reverse proxy configuration via Traefik.

### Creating a New Service

1. **Create service directory** in `hosts/framework-desktop/services/`:

```bash
mkdir hosts/framework-desktop/services/<service-name>
```

2. **Create `default.nix`** with the service module:

```nix
{
  config,
  pkgs,
  fi,
  ...
}:

let
  cfg = config.services.<service-name>;
in

{
  options.services.<service-name> = {
    enable = lib.mkEnableOption "<service-name>";

    # Optional: Service-specific options
    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "Service port";
    };

    # Optional: Traefik configuration
    traefik = {
      enable = lib.mkEnableOption "Traefik reverse proxy";
      domain = lib.mkOption {
        type = lib.types.str;
        default = "<service>.local";
        description = "Domain for reverse proxy";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Enable the NixOS service
    services.<service-name> = {
      enable = true;
      # Service-specific configuration
    };

    # Optional: Persist service data with impermanence
    # See: hosts/framework-desktop/services/module.nix for persist configuration
    fileSystems."<persist-path>" = {
      device = "/persist/<persist-path>";
      fsType = "none";
      options = [ "bind" ];
    };

    # Optional: Traefik configuration
    traefik =
      if cfg.traefik.enable
      then {
        servers.<service-name> = {
          http.address = ":${builtins.toString cfg.port}";
          http.middlewares = [ "retry" ];
          http.routers."${cfg.traefik.domain}" = {
            entryPoints = [ "web" ];
            rule = "Host(`${cfg.traefik.domain}`)";
            service = "<service-name>";
          };
        };
      }
      else { };
  };
}
```

3. **Register the service** in `hosts/framework-desktop/services/module.nix`:

```nix
# Add to the services list
services = [
  # ... existing services
  (import ./<service-name>)
];
```

4. **Enable the service** in `hosts/framework-desktop/configuration.nix`:

```nix
services.<service-name>.enable = true;
```

### Example: Adding a New Service

See `hosts/framework-desktop/services/uptime-kuma/` for a complete example of a service with Traefik configuration.

### Common Service Patterns

**Service with persistence:**

```nix
# Persist data across reboots (impermanence)
fileSystems."/var/lib/<service>" = {
  device = "/persist/var/lib/<service>";
  fsType = "none";
  options = [ "bind" ];
};
```

**Service with Traefik:**

```nix
# Enable reverse proxy
traefik = {
  enable = true;
  domain = "<service>.local";
};
```

**Service without reverse proxy:**

```nix
# Just enable the NixOS service
services.<service-name>.enable = true;
```

## Home Manager Services

Home Manager configurations are in `home/` directory. Each host has its own configuration that imports common modules.

### Adding a Home Manager Package

1. **Edit the host configuration** (e.g., `home/x1-nano/default.nix`):

```nix
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # ... existing packages
    <new-package>
  ];
}
```

2. **Enable the Home Manager service** in the NixOS configuration:

```nix
# In hosts/<host>/configuration.nix
programs.home-manager.enable = true;
```

### Creating a Home Manager Module

For reusable configurations:

1. **Create module in `home/common/`**:

```nix
# home/common/<module-name>/default.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.<module-name>;
in

{
  options.<module-name> = {
    enable = lib.mkEnableOption "<module-name>";
  };

  config = lib.mkIf cfg.enable {
    # Module configuration
  };
}
```

2. **Import the module** in the host configuration:

```nix
{
  imports = [
    ./common/<module-name>
  ];
}
```