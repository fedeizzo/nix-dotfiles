{
  flake.modules.nixos.homelab = { config, ... }: {
    sops = {
      defaultSopsFile = ./homelab-secrets.yaml;

      secrets = {
        homelab-wireguard-private-key = { };

        restic-repository = { };
        restic-password = { };
        local-restic-password = { };
        paperless-admin-password = { };
      };
    };
  };
}
