{ inputs, self, ... }:

let
  system-overlays = {
    default = import ../../../overlays {
      inherit inputs;
    };
    nix-topology = inputs.nix-topology.overlays.default;
  };
in
{
  flake-file.inputs.hermes-agent.url             = "github:NousResearch/hermes-agent";
  flake.nixosConfigurations.homelab = inputs.nixpkg.lib.nixosSystem rec {
    system = "x86_64-linux";

    specialArgs = {
      inherit inputs self;
      hostname = "homelab";
      username = "homelab";
      pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; };
      inherit system-overlays;
    };

    modules = [
      self.modules.nixos.framework-desktop
    ];
  };

  flake.modules.nixos.framework-desktop = {
    imports = [
      inputs.nix-topology.nixosModules.default
      inputs.authentik-nix.nixosModules.default
      inputs.hermes-agent.nixosModules.default

      inputs.self.modules.nixos.misc
      inputs.self.modules.nixos.nix
      inputs.self.modules.nixos.security
      inputs.self.modules.nixos.disk
      inputs.self.modules.nixos.power
      inputs.self.modules.nixos.hardware
      inputs.self.modules.nixos.boot

      inputs.self.modules.nixos.networking
      inputs.self.modules.nixos.wireguard
      inputs.self.modules.nixos.sops
      inputs.self.modules.nixos.environment
      inputs.self.modules.nixos.persistent

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
  };

  flake.deploy.nodes.homelab = {
    hostname = "homelab";
    sshUser = "root";
    sudo = "doas -u";
    sshOpts = [ ];
    magicRollback = true;
    autoRollback = true;
    fastConnection = false;
    remoteBuild = true;
    profiles.system = {
      user = "root";
      path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.homelab;
    };
  };
}
