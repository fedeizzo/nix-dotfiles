# Future Improvements

This document outlines potential enhancements, refactorings, and experimental features for the nix-dotfiles repository. Think of this as a playground for ideas—some may be implemented, others may inspire different approaches.

---

## 🔐 Secrets Management

### Current State
Sops (Secrets Operator) is partially configured but commented out in some hosts.

### Option 1: Sops-Nix Integration

```nix
# nix/modules/secrets/sops.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.fi.secrets;
in
{
  options.fi.secrets.sops = {
    enable = lib.mkEnableOption "sops-based secrets management";
    
    ageKeys = lib.mkOption {
      type = lib.types.path;
      default = "/home/${config.users.users.${config.fi.username}.name}/.config/sops/age/keys.txt";
      description = "Path to age private keys";
    };
    
    sopsFiles = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ./secrets/common.yaml ];
      description = "List of sops-encrypted files";
    };
    
    environmentSecrets = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          path = lib.mkOption { type = lib.types.path; };
          mode = lib.mkOption { type = lib.types.str; default = "0600"; };
          owner = lib.mkOption { type = lib.types.str; };
        };
      });
      description = "Secrets to expose as environment variables";
    };
    
    fileSecrets = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          path = lib.mkOption { type = lib.types.path; };
          mode = lib.mkOption { type = lib.types.str; default = "0600"; };
          owner = lib.mkOption { type = lib.types.str; };
        };
      });
      description = "Secrets to write to files";
    };
  };
  
  config = lib.mkIf cfg.sops.enable {
    imports = [ inputs.sops-nix.nixosModules.sops ];
    
    sops = {
      defaultSopsFile = builtins.head cfg.sops.sopsFiles;
      defaultSopsKeyGenPath = "${pkgs.age}/bin/age-keygen";
      ages.keyFile = cfg.sops.ageKeys;
      
      secrets =
        let
          mkSecret = name: {
            sopsFile = builtins.head cfg.sops.sopsFiles;
            path = "/run/secrets/${name}";
          };
        in
        lib.mapAttrs mkSecret cfg.sops.environmentSecrets;
    };
    
    environment =
      lib.mapAttrs' (name: value: {
        name = "SECRET_${lib.toUpper (lib.replaceStrings [ "-" "_" ] "_" name)}";
        value = "/run/secrets/${name}";
      }) cfg.sops.environmentSecrets;
  };
}
```

### Option 2: HashiCorp Vault

For enterprise-grade secrets management:

```nix
{
  services.vault = {
    enable = true;
    communityPluginPackages = [ pkgs.vault-plugin-secrets-kv ];
    
    serverConfig = {
      listener = {
        tcp = {
          address = "127.0.0.1:8200";
          tls_disable = false;
        };
      }
      
      storage = {
        file = {
          path = "/var/lib/vault/data";
        };
      }
      
      ui = true;
    };
    
    # Auto-unseal with AWS KMS or GCP KMS
    unsealConfig = {
      type = "aws-kms";
      # ... configuration ...
    };
    
    # Dynamic secrets for databases
    extraSecretEngines = {
      databases = {
        type = "database";
        description = "Database secrets";
        options = {
          plugin_name = "postgresql-database-plugin";
        };
        configs = {
          myapp-db = {
            type = "postgresql";
            connection_url = "postgresql://{{username}}:{{password}}@localhost:5432/myapp?sslmode=disable";
            allowed_roles = ["myapp-role"];
            credential_store = {
              username = "vault-user";
              password = "vault-password";
            };
          };
        };
      };
    };
  };
}
```

### Option 3: Doppler / 1Password / CyberArk

Cloud-based secrets management:

```nix
{
  services.doppler = {
    enable = true;
    token = config.fi.secrets.dopplerToken;
    project = "myapp";
    config = "production";
    
    # Inject as environment variables
    envVars = [
      "DATABASE_URL"
      "REDIS_URL"
      "API_KEY"
    ];
  };
}
```

### Option 4: Nix-Managed Secrets

Keep secrets in Nix but encrypt at rest:

```nix
{
  # Store secrets in plain Nix (encrypted in git with git-crypt)
  fi.secrets.plain = {
    postgres = {
      password = "super-secret-password";
      user = "myapp";
      database = "myapp_db";
    };
    
    apiKeys = {
      stripe = "sk_live_...";
      sendgrid = "SG. ...";
    };
  };
  
  # Inject into services
  services.myapp = {
    environment = {
      DATABASE_URL = "postgresql://${fi.secrets.plain.postgres.user}:${fi.secrets.plain.postgres.password}@localhost:5432/${fi.secrets.plain.postgres.database}";
      STRIPE_KEY = fi.secrets.plain.apiKeys.stripe;
    };
  };
}
```

### Option 5: Multi-Host Secret Distribution

```nix
# secrets/hosts/
├── homelab.yaml
├── oven.yaml
├── freezer.yaml
└── shared.yaml  # Shared across all hosts

# Each host has its own age key
# Shared secrets encrypted with multiple recipients
```

---

## 🔗 Service Dependencies

### Advanced Dependency Patterns

#### 1. Resource-Based Dependencies

```nix
{
  name = "myapp";
  
  # Wait for specific resources
  requiresResources = [
    { type = "port"; port = 5432; service = "postgres"; }
    { type = "port"; port = 6379; service = "redis"; }
    { type = "file"; path = "/var/run/postgres/.s.PGSQL.5432"; }
  ];
  
  # Custom readiness probe
  readinessProbe = {
    http = {
      path = "/health";
      port = 3000;
    };
    initialDelaySeconds = 10;
    periodSeconds = 5;
    timeoutSeconds = 3;
    failureThreshold = 3;
  };
}
```

#### 2. Conditional Dependencies

```nix
{
  name = "myapp";
  
  # Only require these if feature is enabled
  requiresIf = {
    "enableEmail" = [ "mailhog.service" ];
    "enableCache" = [ "redis.service" ];
    "enableSearch" = [ "elasticsearch.service" ];
  };
}
```

#### 3. Dependency Graph Visualization

```nix
# scripts/dep-graph.sh
#!/usr/bin/env bash

# Generate dependency graph in DOT format
nix-instantiate --eval -E '
  let
    services = import ./hosts/homelab/services/default.nix;
  in
  builtins.toJSON {
    nodes = map (s: s.name) services.services;
    edges = map (s: {
      from = s.name;
      to = s.requires or [];
    }) services.services;
  }
' | jq -r '
  "digraph dependencies {
    rankdir=LR;
    ${.nodes[] | "  \"\(.)\";"}
    ${.edges[] | "  \"\( .from)\" -> \"\( .to | join("\", \"") )\";"}
  }"
' > dependencies.dot

# Visualize with graphviz
dot -Tpng dependencies.dot -o dependencies.png
```

#### 4. Service Startup Order Enforcement

```nix
{
  # Create a custom target for service groups
  systemd.targets.service-group = {
    description = "Service Group Target";
    wantedBy = [ "multi-user.target" ];
  };
  
  # Group related services
  fi.services.group = {
    database = [ "postgres" "redis" "minio" ];
    web = [ "nginx" "myapp" "api" ];
    background = [ "worker" "scheduler" "queue" ];
  };
  
  # Ensure groups start in order
  systemd.services.database-group = {
    wantedBy = [ "service-group.target" ];
    requires = map (s: "${s}.service") config.fi.services.group.database;
    after = config.fi.services.group.database;
  };
}
```

#### 5. Dependency Resolution Algorithm

```nix
# nix/modules/services/dependencies.nix
{ config, lib, ... }:

let
  # Topological sort of service dependencies
  topologicalSort = services:
    let
      graph = lib.listToAttrs (
        map (s: {
          name = s.name;
          value = s.requires or [];
        })
        services
      );
      
      visit = node: visited: stack:
        if builtins.elem node visited then stack
        else
          let
            deps = graph.${node} or [];
            visited' = node:visited;
            stack' = lib.foldl (s: dep: visit dep visited' s) stack deps;
          in
          (node:stack');
    in
    lib.foldl (s: svc: visit svc.name [] s) [] services;
in
{
  options.fi.services.dependencyResolution = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Automatically resolve and validate service dependencies";
  };
  
  config = lib.mkIf config.fi.services.dependencyResolution {
    # Validate no circular dependencies
    _module.check = {
      assertions = [
        {
          assertion = !(lib.any (s: s: lib.any (dep: dep == s.name) (s.requires or [])) services);
          message = "Circular dependency detected in services";
        }
      ];
    };
  };
}
```

---

## 🏥 Health Checks

### Option 1: Uptime Kuma (Declarative)

```nix
{
  services.uptime-kuma = {
    enable = true;
    port = 3001;
    user = "uptime-kuma";
    dataDir = "/var/lib/uptime-kuma";
    
    # Declarative monitor configuration
    config.monitors = [
      {
        id = "website-main";
        name = "Main Website";
        type = "http";
        url = "https://example.com";
        interval = 60;
        timeout = 30;
        keyword = "Welcome";
        statusPage = "main";
        pushInterval = 60;
        heartbeat = true;
      }
      {
        id = "db-postgres";
        name = "PostgreSQL";
        type = "postgres";
        host = "localhost";
        port = 5432;
        database = "myapp";
        username = "myapp";
        interval = 30;
        statusPage = "infrastructure";
      }
      {
        id = "cache-redis";
        name = "Redis Cache";
        type = "redis";
        host = "localhost";
        port = 6379;
        interval = 30;
        statusPage = "infrastructure";
      }
      {
        id = "api-app";
        name = "API Service";
        type = "port";
        host = "localhost";
        port = 3000;
        interval = 60;
        statusPage = "services";
      }
      {
        id = "external-google";
        name = "Google DNS";
        type = "dns";
        host = "8.8.8.8";
        interval = 300;
        statusPage = "external";
      }
    ];
    
    config.statusPages = [
      {
        id = "main";
        title = "Main Services";
        icon = "🌐";
        monitors = [ "website-main" "api-app" ];
      }
      {
        id = "infrastructure";
        title = "Infrastructure";
        icon = "🏗️";
        monitors = [ "db-postgres" "cache-redis" ];
      }
    ];
  };
}
```

### Option 2: Prometheus + Grafana

```nix
{
  services.prometheus = {
    enable = true;
    port = 9090;
    
    # Service exporters
    exporters = {
      node = {
        enable = true;
        port = 9100;
      };
      
      postgresql = {
        enable = true;
        port = 9187;
        dataSources = [
          {
            name = "myapp";
            dataSource = "postgresql://myapp:password@localhost:5432/myapp?sslmode=disable";
          }
        ];
      };
      
      redis = {
        enable = true;
        port = 9121;
      };
      
      nginx = {
        enable = true;
        port = 9113;
      };
      
      blackbox = {
        enable = true;
        port = 9115;
      };
    };
    
    # Alert rules
    alertmanagers = [
      {
        port = 9093;
        config = {
          global = {
            slack_api_url = config.fi.secrets.slackWebhook;
          };
          
          route = {
            receiver = "slack";
            group_by = [ "alertname" "cluster" "service" ];
            group_wait = "30s";
            group_interval = "5m";
            repeat_interval = "4h";
          };
          
          receivers = [
            {
              name = "slack";
              slack_configs = [
                {
                  channel = "#alerts";
                  title = "{{ .GroupLabels.alertname }}";
                  text = "{{ range .Alerts }}{{ .Annotations.description }}\n{{ end }}";
                }
              ];
            }
            {
              name = "email";
              email_configs = [
                {
                  to = "admin@example.com";
                  send_resolved = true;
                }
              ];
            }
          ];
        };
      }
    ];
    
    # Recording rules
    ruleFiles = {
      alerts = ''
        groups:
          - name: service_alerts
            rules:
              - alert: HighErrorRate
                expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.1
                for: 5m
                labels:
                  severity: critical
                annotations:
                  summary: High error rate detected
                  
              - alert: ServiceDown
                expr: up == 0
                for: 1m
                labels:
                  severity: critical
                  annotations:
                    summary: Service {{ $labels.job }} is down
      '';
    };
  };
  
  services.grafana = {
    enable = true;
    port = 3000;
    adminUser = "admin";
    adminPasswordFile = config.secrets.grafanaPassword;
    
    # Datasources
    settings.datasources = {
      prometheus = {
        url = "http://localhost:9090";
        isDefault = true;
        access = "proxy";
      };
    };
    
    # Dashboards
    dashboards = {
      services = {
        "nginx.json" = builtins.readFile ./grafana/nginx-dashboard.json;
        "postgres.json" = builtins.readFile ./grafana/postgres-dashboard.json;
        "system.json" = builtins.readFile ./grafana/system-dashboard.json;
      };
    };
  };
}
```

### Option 3: Checkmk Raw Edition

```nix
{
  services.check-mk = {
    enable = true;
    
    # Service discovery
    serviceDiscovery = {
      enable = true;
      interval = 300;
    };
    
    # Custom checks
    extraCheckModules = {
      "myapp" = ''
        check_items = {
            ("myapp_health", "HTTP", "http://localhost:3000/health"),
        }
        check_function = myapp_check
        info_table = []
        
        def myapp_check(param, items):
            try:
                response = urllib.request.urlopen(items[2])
                if response.status == 200:
                    return (0, "Service healthy")
                else:
                    return (2, "Service returned status {}".format(response.status))
            except Exception as e:
                return (2, "Connection failed: {}".format(str(e)))
      '';
    };
  };
}
```

### Option 4: Custom Nix-Based Health Checker

```nix
# home/common/scripts/health-checker/default.nix
{ pkgs, lib, ... }:

let
  healthChecks = [
    {
      name = "nginx";
      type = "tcp";
      host = "localhost";
      port = 80;
      timeout = 5;
    }
    {
      name = "postgres";
      type = "tcp";
      host = "localhost";
      port = 5432;
      timeout = 5;
    }
    {
      name = "redis";
      type = "tcp";
      host = "localhost";
      port = 6379;
      timeout = 5;
    }
    {
      name = "api";
      type = "http";
      url = "http://localhost:3000/health";
      expectedStatus = 200;
      timeout = 10;
    }
  ];
  
  healthCheckScript = pkgs.writeShellScriptBin "health-checker" ''
    #!/usr/bin/env bash
    
    set -euo pipefail
    
    COLORS=(
      [0]="\033[0;32m"  # green - OK
      [1]="\033[0;33m"  # yellow - warning
      [2]="\033[0;31m"  # red - critical
      [3]="\033[0m"     # reset
    )
    
    check_tcp() {
      local host=$1 port=$2 timeout=$3
      if nc -z -w "$timeout" "$host" "$port" 2>/dev/null; then
        return 0
      else
        return 1
      fi
    }
    
    check_http() {
      local url=$1 expected=$2 timeout=$3
      local status
      status=$(curl -s -o /dev/null -w "%{http_code}" --max-time "$timeout" "$url" 2>/dev/null || echo "000")
      if [[ "$status" == "$expected" ]]; then
        return 0
      else
        return 1
      fi
    }
    
    echo "=== Health Check Report ==="
    echo "Timestamp: $(date)"
    echo ""
    
    failed=0
    
    ${lib.concatMapStrings (check: ''
      echo -en "${COLORS[0]}Checking ${check.name}...${COLORS[3]} "
      if ${
        if check.type == "tcp" then
          "check_tcp \"${check.host}\" ${check.port} ${check.timeout}"
        else
          "check_http \"${check.url}\" ${check.expectedStatus} ${check.timeout}"
        fi
      }; then
        echo "✓ OK"
      else
        echo -en "${COLORS[2]}✗ FAILED${COLORS[3]}"
        failed=1
      fi
    '') healthChecks}
    
    echo ""
    if [[ $failed -eq 0 ]]; then
      echo "All checks passed!"
      exit 0
    else
      echo "Some checks failed!"
      exit 1
    fi
  '';
in
{
  programs.fish.shellAliases = {
    health = "${healthCheckScript}/bin/health-checker";
  };
  
  # Systemd service for periodic health checks
  systemd.user.services.health-checker = {
    Unit = {
      Description = "Periodic health checker";
      After = [ "network.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${healthCheckScript}/bin/health-checker";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  
  # Timer for periodic checks
  systemd.user.timers.health-checker = {
    Unit = {
      Description = "Timer for health checker";
    };
    Timer = {
      OnBootSec = "5min";
      OnUnitActiveSec = "5min";
      Unit = "health-checker.service";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
```

### Option 5: Synthetics Monitoring (Elastic APM Style)

```nix
{
  services.synthetics = {
    enable = true;
    agentConfig = {
      environment = "production";
      serviceName = "nix-dotfiles-monitoring";
    };
    
    # Synthetic tests
    tests = {
      website = {
        schedule = "*/5 * * * *";  # Every 5 minutes
        locations = [ "aws:oregon" "gcp:virginia" ];
        
        test = {
          name = "Main Website";
          type = "browser";
          script = ''
            const { steps, expect } = require('@elastic/synthetics');
            
            step('Load homepage', async () => {
              await page.goto('https://example.com');
              await expect(page).toHaveTitle(/Welcome/);
            });
            
            step('Check critical elements', async () => {
              await expect(page.locator('h1')).toBeVisible();
              await expect(page.locator('.nav')).toBeVisible();
            });
          '';
        };
      };
      
      api = {
        schedule = "*/1 * * * *";  # Every minute
        test = {
          name = "API Health";
          type = "check";
          request = {
            url = "https://api.example.com/health";
            method = "GET";
            headers = {
              "Accept" = "application/json";
            };
          };
          assertions = [
            {
              type = "status";
              value = 200;
            }
            {
              type = "jsonPath";
              path = "$.status";
              value = "healthy";
            }
          ];
        };
      };
    };
  };
}
```

### Option 6: Distributed Tracing

```nix
{
  services.jaeger = {
    enable = true;
    port = 16686;
    
    collector = {
      port = 14268;
      zipkinPort = 9411;
    };
  };
  
  # Instrument services with OpenTelemetry
  services.myapp.opentelemetry = {
    enable = true;
    exporter = {
      type = "jaeger";
      endpoint = "localhost:14268";
    };
    serviceName = "myapp";
    samplingRate = 0.1;  # 10% of traces
  };
}
```

---

## 🧪 Testing Infrastructure

### Comprehensive Test Suite

```
tests/
├── default.nix                      # Test suite aggregator
├── flake-check.nix                  # nix flake check validation
├── home-manager.nix                 # Home Manager config validation
├── nixos/
│   ├── default.nix
│   ├── hostname-validation.nix      # Validate hostname configuration
│   ├── user-existence.nix           # Ensure users exist
│   ├── port-availability.nix        # Check for port conflicts
│   └── service-configuration.nix    # Validate service configs
├── services/
│   ├── default.nix
│   ├── nginx/
│   │   ├── config.nix               # Validate nginx config
│   │   └── connectivity.nix         # Test nginx connectivity
│   ├── postgres/
│   │   ├── config.nix
│   │   └── replication.nix          # Test replication if enabled
│   ├── redis/
│   │   └── persistence.nix          # Test RDB/AOF persistence
│   └── common/
│       ├── health-check.nix         # Generic health check
│       └── security.nix             # Security hardening checks
├── integration/
│   ├── default.nix
│   ├── service-dependencies.nix     # Test service startup order
│   ├── reverse-proxy.nix            # Test reverse proxy routing
│   ├── database-migrations.nix      # Test DB migrations
│   └── backup-restore.nix           # Test backup/restore workflow
├── security/
│   ├── default.nix
│   ├── ssh-hardening.nix            # Validate SSH config
│   ├── firewall.nix                 # Test firewall rules
│   └── secrets.nix                  # Ensure no secrets in plain text
└── performance/
    ├── default.nix
    ├── boot-time.nix                # Measure boot time
    └── service-startup.nix          # Measure service startup times
```

### Example Test Implementations

#### Port Conflict Detection

```nix
# tests/nixos/port-availability.nix
{ pkgs, lib, ... }:

{
  tests.port-availability = pkgs.runCommand "port-availability-test"
    {
      buildInputs = [ pkgs.nix ];
    }
    ''
      #!/usr/bin/env bash
      
      # Extract all service port configurations
      PORTS=$(nix-instantiate --eval -E '
        let
          services = import ../../hosts/*/services/default.nix;
        in
        builtins.toJSON (
          map (s: {
            name = s.name;
            port = s.port;
            protocol = s.protocol or "tcp";
          })
          services.services
        )
      ' | jq -r '.[] | "\(.name):\(.port):\(.protocol)"')
      
      # Check for duplicate ports
      DUPLICATES=$(echo "$PORTS" | sort | uniq -d)
      
      if [[ -n "$DUPLICATES" ]]; then
        echo "ERROR: Duplicate ports detected:"
        echo "$DUPLICATES"
        exit 1
      fi
      
      # Check for reserved ports (< 1024)
      RESERVED=$(echo "$PORTS" | awk -F: '$2 < 1024 {print $0}')
      
      if [[ -n "$RESERVED" ]]; then
        echo "WARNING: Using reserved ports (requires root):"
        echo "$RESERVED"
      fi
      
      touch "$out"
    '';
}
```

#### Service Configuration Validation

```nix
# tests/services/nginx/config.nix
{ pkgs, lib, ... }:

{
  tests.services.nginx.config = pkgs.runCommand "nginx-config-test"
    {
      buildInputs = [ pkgs.nginx pkgs.coreutils ];
    }
    ''
      #!/usr/bin/env bash
      
      set -euo pipefail
      
      # Generate nginx configuration
      nginx -t -c /dev/null <<EOF
      events { worker_connections 1024; }
      
      http {
        ${builtins.readFile ../../hosts/homelab/services/nginx/nginx.conf}
      }
      EOF
      
      touch "$out"
    '';
}
```

#### Backup and Restore Testing

```nix
# tests/integration/backup-restore.nix
{ pkgs, lib, config, ... }:

{
  tests.integration.backup-restore = pkgs.runCommand "backup-restore-test"
    {
      nativeBuildInputs = [ pkgs.bash pkgs.coreutils pkgs.gnutar ];
      inherit (config) system;
    }
    ''
      #!/usr/bin/env bash
      
      set -euo pipefail
      
      # Create test data
      TEST_DIR="/tmp/backup-test-$$"
      mkdir -p "$TEST_DIR/data"
      echo "test data" > "$TEST_DIR/data/file1.txt"
      echo "more test data" > "$TEST_DIR/data/file2.txt"
      
      # Simulate backup
      tar -czf "$TEST_DIR/backup.tar.gz" -C "$TEST_DIR" data
      
      # Verify backup integrity
      tar -tzf "$TEST_DIR/backup.tar.gz" > /dev/null
      
      # Simulate restore
      RESTORE_DIR="/tmp/restore-test-$$"
      mkdir -p "$RESTORE_DIR"
      tar -xzf "$TEST_DIR/backup.tar.gz" -C "$RESTORE_DIR"
      
      # Verify restored data
      diff -r "$TEST_DIR/data" "$RESTORE_DIR/data"
      
      # Cleanup
      rm -rf "$TEST_DIR" "$RESTORE_DIR"
      
      touch "$out"
    '';
}
```

#### Security Validation Tests

```nix
# tests/security/secrets.nix
{ pkgs, ... }:

{
  tests.security.no-plaintext-secrets = pkgs.runCommand "secrets-validation"
    {
      buildInputs = [ pkgs.grep pkgs.coreutils ];
    }
    ''
      #!/usr/bin/env bash
      
      set -euo pipefail
      
      # Patterns that might indicate plaintext secrets
      PATTERNS=(
        'password\s*=\s*["\047][^"\047]+["\047]'
        'api_key\s*=\s*["\047][^"\047]+["\047]'
        'secret\s*=\s*["\047][^"\047]+["\047]'
        'token\s*=\s*["\047][^"\047]+["\047]'
      )
      
      for pattern in "${pkgs.coreutils}/bin/printf" "%s\n" "${PATTERNS[@]}"; do
        if grep -rE "$pattern" hosts/ home/ --include="*.nix" 2>/dev/null; then
          echo "ERROR: Potential plaintext secret found!"
          exit 1
        fi
      done
      
      touch "$out"
    '';
}
```

### CI/CD Pipeline

```yaml
# .github/workflows/nix-validation.yml
name: Nix Validation

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]
  schedule:
    - cron: '0 0 * * *'  # Daily at midnight

env:
  NIX_CONFIG: |
    extra-experimental-features = nix-command flakes
    extra-substituters = https://cache.nixos.org
    extra-trusted-public-keys = cache.nixos.org-1:6NECHQW20J4qWWwQRs6sMz5EX13NCqeo2s+D05nFIw=

jobs:
  flake-check:
    name: Flake Check (${{ matrix.system }})
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        system: [x86_64-linux, x86_64-darwin]
        exclude:
          - os: ubuntu-latest
            system: x86_64-darwin
          - os: macos-latest
            system: x86_64-linux
    
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
        with:
          extra-conf: |
            experimental-features = nix-command flakes
            extra-trusted-public-keys = cache.nixos.org-1:6NECHQW20J4qWWwQRs6sMz5EX13NCqeo2s+D05nFIw=
      
      - name: Cache Nix
        uses: DeterminateSystems/magic-nix-cache-action@main
      
      - name: Check Flake
        run: nix flake check
      
      - name: Build NixOS Configuration
        run: nix build .#nixosConfigurations.homelab.config.system.build.toplevel
        if: matrix.system == 'x86_64-linux'
      
      - name: Build macOS Configuration
        run: nix build .#darwinConfigurations.macbook-pro.config.darwin.system.stateVersion
        if: matrix.system == 'x86_64-darwin'

  home-manager:
    name: Home Manager Check
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Check Home Manager
        run: home-manager --flake .#homelab --impure --no-build
      
      - name: Check Home Manager (Linux)
        run: home-manager --flake .#oven --impure --no-build
      
      - name: Check Home Manager (macOS)
        run: home-manager --flake .#macbook-pro --impure --no-build

  security-scan:
    name: Security Scan
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Run Security Tests
        run: nix flake check --override-input nixpkgs nixpkgs#master
      
      - name: Check for Secrets
        run: |
          if grep -rE '(password|api_key|secret|token)\s*=\s*["\047][^"\047]+["\047]' hosts/ home/ --include="*.nix"; then
            echo "Potential plaintext secrets found!"
            exit 1
          fi

  integration-tests:
    name: Integration Tests
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Run Integration Tests
        run: |
          nix develop --command bash -c '
            echo "Running service dependency tests..."
            nix-shell tests/integration/service-dependencies.nix
            echo "Running backup/restore tests..."
            nix-shell tests/integration/backup-restore.nix
          '

  documentation:
    name: Documentation Check
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Nix
        uses: DeterminateSystems/nix-installer-action@main
      
      - name: Check Documentation Links
        run: |
          nix develop --command bash -c '
            # Check for broken markdown links
            mdlinkcheck docs/
            
            # Verify all files in SUMMARY.md exist
            for file in $(grep -oP '\[\K[^]]+(?=\])' docs/src/SUMMARY.md); do
              if [[ ! -f "docs/src/$file" ]]; then
                echo "ERROR: Missing documentation file: $file"
                exit 1
              fi
            done
          '

  notify:
    name: Notify on Failure
    needs: [flake-check, home-manager, security-scan, integration-tests, documentation]
    if: failure()
    runs-on: ubuntu-latest
    steps:
      - name: Send Slack Notification
        uses: slackapi/slack-github-action@v1.24.0
        with:
          payload: |
            {
              "text": "🚨 Nix Dotfiles CI Failed",
              "attachments": [{
                "color": "danger",
                "text": "One or more validation checks failed. Please review the CI output."
              }]
            }
        env:
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
```

### Local Testing Helpers

```nix
# scripts/test/default.nix
{ pkgs, ... }:

{
  programs.bash.shellAliases = {
    # Run all tests
    "test-all" = "${pkgs.writeShellScript "test-all" ''
      #!/usr/bin/env bash
      set -euo pipefail
      
      echo "=== Running Nix Flake Check ==="
      nix flake check
      
      echo "=== Running Home Manager Check ==="
      home-manager --flake .#homelab --impure --no-build
      
      echo "=== Running Service Tests ==="
      for test in tests/services/*/; do
        echo "Testing $test..."
        nix-build "$test"
      done
      
      echo "=== All tests passed! ==="
    ''}";
    
    # Test specific service
    "test-service" = "${pkgs.writeShellScript "test-service" ''
      #!/usr/bin/env bash
      set -euo pipefail
      
      SERVICE=$1
      if [[ -z "$SERVICE" ]]; then
        echo "Usage: test-service <service-name>"
        exit 1
      fi
      
      echo "Testing $SERVICE..."
      nix-build "tests/services/$SERVICE/"
    ''}";
    
    # Quick config check
    "check" = "${pkgs.writeShellScript "check" ''
      #!/usr/bin/env bash
      nix flake check --no-update-lockfile
    ''}";
    
    # Dry-run deployment
    "dry-run" = "${pkgs.writeShellScript "dry-run" ''
      #!/usr/bin/env bash
      nixos-rebuild switch --flake .#homelab --impure --dry-run
    ''}";
    
    # Test rollback
    "rollback-test" = "${pkgs.writeShellScript "rollback-test" ''
      #!/usr/bin/env bash
      # Generate new config without applying
      nixos-rebuild build --flake .#homelab
      echo "New config generated. Available generations:"
      nix-env -q --generation
    ''}";
  };
  
  # Nix shell for testing
  development.shells.test = pkgs.mkShell {
    buildInputs = [
      pkgs.nix
      pkgs.home-manager
      pkgs.nginx
      pkgs.postgresql
      pkgs.redis
      pkgs.curl
      pkgs.wget
      pkgs.jq
      pkgs.gnused
    ];
    
    shellHook = ''
      echo "Testing environment ready!"
      echo "Use 'test-all' to run all tests"
      echo "Use 'check' to validate configuration"
    '';
  };
}
```

---

## 🚀 Additional Ideas

### 1. GitOps Integration

```nix
# Integrate with FluxCD or ArgoCD for cluster deployments
{
  services.gitops = {
    enable = true;
    
    # Generate Kubernetes manifests
    kubernetes = {
      enable = true;
      outputDir = "./k8s";
      
      # Deploy configurations
      deployments = [
        {
          name = "myapp";
          namespace = "production";
          replicas = 3;
        }
      ];
    };
  };
}
```

### 2. Multi-Cloud Deployments

```nix
{
  # Deploy same configuration to AWS, GCP, Azure
  fi.cloud = {
    providers = [ "aws" "gcp" "azure" ];
    
    # Cloud-specific configurations
    aws = {
      instanceType = "t3.medium";
      ami = "ami-...";
      vpc = "main";
    };
    
    gcp = {
      machineType = "e2-medium";
      zone = "us-central1-a";
    };
  };
}
```

### 3. Infrastructure as Code Testing

```nix
{
  # Terraform integration
  fi.terraform = {
    enable = true;
    
    # Validate Terraform configs
    validate = {
      enable = true;
      hooks = [ "pre-commit" ];
    };
    
    # Generate Terraform from Nix
    generate = {
      enable = true;
      output = "./terraform";
    };
  };
}
```

### 4. Configuration Drift Detection

```nix
{
  services.drift-detection = {
    enable = true;
    interval = "weekly";
    
    # Track configuration changes
    tracking = {
      packages = true;
      services = true;
      users = true;
      files = [
        "/etc/nginx/nginx.conf"
        "/etc/postgresql/postgresql.conf"
      ];
    };
    
    # Alert on drift
    alerts = {
      slack = true;
      email = true;
    };
  };
}
```

### 5. Automated Documentation Generation

```nix
{
  # Auto-generate documentation from Nix configs
  fi.documentation = {
    enable = true;
    
    # Generate service documentation
    services = {
      enable = true;
      output = "./docs/services.md";
      
      # Include in each service
      fields = [ "name" "port" "description" "requires" ];
    };
    
    # Generate architecture diagrams
    diagrams = {
      enable = true;
      output = "./docs/architecture/";
      
      # Service dependency graph
      dependencies = true;
      
      # Network topology
      topology = true;
    };
  };
}
```

### 6. A/B Testing Framework

```nix
{
  # Test new configurations before full rollout
  fi.ab-testing = {
    enable = true;
    
    # Define test groups
    groups = {
      canary = {
        hosts = [ "oven" ];
        percentage = 10;
      };
      beta = {
        hosts = [ "homelab" ];
        percentage = 50;
      };
      stable = {
        hosts = [ "freezer" "macbook-pro" ];
        percentage = 100;
      };
    };
    
    # Rollout strategy
    rollout = {
      strategy = "canary";
      step = 10;  # 10% increments
      interval = "daily";
    };
  };
}
```

### 7. Configuration Templates

```nix
{
  # Reusable configuration templates
  fi.templates = {
    # Service templates
    services = {
      webapp = {
        port = 3000;
        requires = [ "nginx" ];
        healthCheck = "/health";
      };
      
      api = {
        port = 8080;
        requires = [ "database" "cache" ];
      };
      
      worker = {
        requires = [ "queue" "database" ];
      };
    };
    
    # Host templates
    hosts = {
      minimal = {
        # Minimal configuration
      };
      
      full = {
        # Full desktop environment
      };
      
      server = {
        # Server-only configuration
      };
    };
  };
}
```

### 8. Configuration Versioning

```nix
{
  # Track configuration versions
  fi.versioning = {
    enable = true;
    
    # Semantic versioning
    version = "1.2.3";
    
    # Migration scripts
    migrations = {
      "1.0.0" = ./migrations/1.0.0.nix;
      "1.1.0" = ./migrations/1.1.0.nix;
      "1.2.0" = ./migrations/1.2.0.nix;
    };
    
    # Auto-apply migrations
    autoMigrate = true;
  };
}
```

### 9. Performance Optimization

```nix
{
  # Performance tuning
  fi.performance = {
    # Nix optimization
    nix = {
      parallelDownloads = 10;
      cores = 0;  # All cores
      maxJobs = "auto";
    };
    
    # System optimization
    system = {
      # Enable zram
      zram = true;
      
      # SSD optimization
      ssdOptimization = true;
      
      # CPU governor
      governor = "performance";
    };
    
    # Service optimization
    services = {
      # Connection pooling
      postgresql = {
        maxConnections = 200;
        sharedBuffers = "2GB";
      };
      
      # Caching
      nginx = {
        enableCache = true;
        cacheSize = "10GB";
      };
    };
  };
}
```

### 10. Disaster Recovery

```nix
{
  # Comprehensive disaster recovery
  fi.disaster-recovery = {
    enable = true;
    
    # Backup strategy
    backups = {
      # Frequency
      schedule = "daily";
      retention = "30d";
      
      # Targets
      targets = [
        {
          name = "local";
          path = "/backup";
        }
        {
          name = "remote";
          path = "s3://backup-bucket";
        }
        {
          name = "offsite";
          path = "ssh://backup-server/backup";
        }
      ];
      
      # What to backup
      include = [
        "/home"
        "/var/lib/postgresql"
        "/var/lib/redis"
        "/etc"
      ];
    };
    
    # Recovery procedures
    recovery = {
      # Automated recovery scripts
      scripts = ./recovery-scripts;
      
      # Recovery time objective (RTO)
      rto = "4h";
      
      # Recovery point objective (RPO)
      rpo = "24h";
    };
  };
}
```

---

## 🎯 Prioritization Matrix

| Feature | Impact | Effort | Priority |
|---------|--------|--------|----------|
| Service Dependencies | High | Low | 🔥 High |
| Secrets Management | High | Medium | 🔥 High |
| Health Checks | Medium | Medium | ⭐ Medium |
| Testing Infrastructure | High | High | ⭐ Medium |
| Documentation Generation | Medium | Low | 🌱 Low |
| Disaster Recovery | High | High | 🌱 Low |
| Multi-Cloud | Low | Very High | 🌱 Low |

---

## 📝 Notes

- **Start small**: Pick one feature and implement it fully
- **Test as you go**: Don't skip testing when adding new features
- **Document everything**: Update docs as you implement
- **Iterate**: First implementation doesn't need to be perfect
- **Community**: Share ideas and get feedback from others

---

*This document is a living document. Feel free to add more ideas, refine existing ones, or implement any of these concepts!*