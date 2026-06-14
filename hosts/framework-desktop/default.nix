{ inputs, ... }:

{
  imports = [
    inputs.nix-topology.nixosModules.default
    inputs.authentik-nix.nixosModules.default
    # inputs.climbing-lab.nixosModules.default
    inputs.hermes-agent.nixosModules.default

    ./system/backup.nix
    ./system/boot.nix
    ./system/deployment.nix
    ./system/disko.nix
    ./system/hardware.nix
    ./system/misc.nix
    ./system/networking.nix
    ./system/nix.nix
    ./system/persistent.nix
    ./system/security.nix
    ./system/sops.nix
    ./system/users.nix
    inputs.self.modules.nixos.starship

    inputs.self.modules.nixos.fi-services
    inputs.self.modules.nixos.fedeizzo-dev
    inputs.self.modules.nixos.nix-dotfiles-docs
    inputs.self.modules.nixos.streaming
    inputs.self.modules.nixos.nextcloud
    inputs.self.modules.nixos.affine
    inputs.self.modules.nixos.immich
    inputs.self.modules.nixos.paperless
    inputs.self.modules.nixos.fusion
    inputs.self.modules.nixos.calibre
    inputs.self.modules.nixos.llama-swap
    inputs.self.modules.nixos.lemonade
    inputs.self.modules.nixos.ntfy
    inputs.self.modules.nixos.open-webui
    inputs.self.modules.nixos.hermes
    inputs.self.modules.nixos.backrest
    inputs.self.modules.nixos.n8n
    inputs.self.modules.nixos.neo4j
    inputs.self.modules.nixos.searx
    inputs.self.modules.nixos.grafana
    inputs.self.modules.nixos.gatus
    inputs.self.modules.nixos.influxdb
    inputs.self.modules.nixos.logrotate
    inputs.self.modules.nixos.prometheus
    inputs.self.modules.nixos.glance
    inputs.self.modules.nixos.hass
    inputs.self.modules.nixos.authentik
    inputs.self.modules.nixos.blocky
    inputs.self.modules.nixos.garmindb
    inputs.self.modules.nixos.net-worth
    inputs.self.modules.nixos.postgres
    inputs.self.modules.nixos.traefik
  ];
}
