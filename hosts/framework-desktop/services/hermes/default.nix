{ config, pkgs, lib, ... }:

{
  services.hermes-agent = {
    enable = true;
    container.enable = lib.mkForce false;

    user = "hermes";
    group = "hermes";
    createUser = false;

    settings = {
      model = {
        default = "qwen36-35b-a3b";
        provider = "custom";
        base_url = "https://llama.fedeizzo.dev/v1";
      };
      stt = {
        enabled = true;
        provider = "openai";
        openai = {
          model = "Whisper-Large-v3-Turbo";
        };
      };

      memory = {
        memory_enable = true;
        user_profile_enable = true;
      };
    };

    documents = {
      "SOUL.md" = ./SOUL.md;
      # "USER.md" = ./documents/USER.md;
    };

    environmentFiles = [ config.sops.secrets.hermes-env.path ];
    addToSystemPackages = true;

    # Inject proxy configuration into the Hermes environment for tool calls
    environment = {
      HTTP_PROXY = "http://127.0.0.1:3128";
      HTTPS_PROXY = "http://127.0.0.1:3128";
      OPENAI_API_KEY = "placeholder";
      STT_OPENAI_API_KEY = "placeholder";
      STT_OPENAI_MODEL = "Whisper-Large-v3-Turbo";
      STT_OPENAI_BASE_URL = "https://lemonade.fedeizzo.dev/v1";
    };
  };

  # Systemd Hardening (Least Privilege Sandboxing)
  systemd.services.hermes-agent.serviceConfig = {
    # Mount the entire OS filesystem as read-only
    ProtectHome = lib.mkForce true; # Deny access to /home, protecting user data from local leaks
    PrivateTmp = lib.mkForce true; # Mount a separate, isolated /tmp directory
    NoNewPrivileges = lib.mkForce true; # Prevent the process and its children from gaining new privileges
    IPAddressDeny = "any"; # Block all direct outbound network access at the systemd/kernel level
    IPAddressAllow = [ "127.0.0.1/32" "::1/128" ]; # Allow outbound access ONLY to localhost (for Squid Proxy and local Signal REST API)
  };

  # Egress Filtering via Squid Proxy
  services.squid = {
    enable = true;

    proxyAddress = "127.0.0.1";
    proxyPort = 3128;

    extraConfig = ''
      # 1. Define ACL for whitelisted domains the agent is allowed to access
      acl whitelist dstdomain .github.com
      acl whitelist dstdomain .fedeizzo.dev
      acl whitelist dstdomain .openrouter.ai

      # 2. Deny any request that is NOT in the whitelist.
      # Valid traffic (matches the whitelist) will simply bypass this rule
      # and be allowed by the default 'http_access allow localhost' below it.
      http_access deny !whitelist
    '';
  };

  # Local Signal CLI Daemon for E2EE Communication
  systemd.services.signal-cli = {
    description = "Signal CLI Daemon";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    serviceConfig = {
      User = "hermes";
      Group = "hermes";
      ExecStart = "${pkgs.signal-cli}/bin/signal-cli --config /var/lib/signal-cli daemon --http 127.0.0.1:7583";
      Restart = "always";
      RestartSec = 5;
    };
  };

  # Ensure the state directory for the Signal CLI exists on the host
  systemd.tmpfiles.rules = [
    "d /var/lib/signal-cli 0770 hermes hermes - -"
  ];

  sops.secrets.hermes-env = {
    format = "dotenv";
    mode = "0400";
    # Automatically restart the service when the secret changes
    restartUnits = [ "hermes-agent.service" ];
    sopsFile = ./hermes-homelab-secrets.env;
    key = ""; # Maps the entire env file as a secret
  };
}
