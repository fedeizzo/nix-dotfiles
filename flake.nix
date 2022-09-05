{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    deploy-rs.url = "github:serokell/deploy-rs";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/6b4445aa659fa26b4f36d9975b34632312699a85";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    hyprwm-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, deploy-rs, home-manager, ... }@inputs:
    let
      config = {
        username = "fedeizzo";
        hostname = "fedeizzo-nixos";
      };
    in
    {
      # by default the configuration used for nixos-rebuild switch
      # is matched with the current hostname
      nixosConfigurations.${config.hostname} = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        specialArgs = { inherit inputs; };

        modules =
          let
            defaults = { pkgs, ... }: {
              _module.args.nixpkgs-unstable = import inputs.nixpkgs-unstable {
                inherit (pkgs.stdenv.targetPlatform) system;
              };
              _module.args.nixpkgs-old = import inputs.nixpkgs-old {
                inherit (pkgs.stdenv.targetPlatform) system;
              };
            };
          in
          ([
            defaults
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                inputs.emacs-overlay.overlay
                (self: super: {
                  waybar = super.waybar.overrideAttrs (oldAttrs: {
                    mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
                  });
                })
              ];
            })
            {
              options = with nixpkgs.lib; {
                username = mkOption {
                  type = types.str;
                  description = "The username to use";
                };
                hostname = mkOption {
                  type = types.str;
                  description = "The hostname of the machine";
                };
              };
              inherit config;
            }
            ./system/configuration.nix
            home-manager.nixosModules.home-manager
            ./home/configuration.nix
          ]);
      };

      nixosConfigurations.rasp-nixos = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";

        specialArgs = { inherit inputs; };
        modules = [
          nixos-hardware.nixosModules.raspberry-pi-4
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              (self: super: {
                tailscalewithnginx = super.tailscale.overrideAttrs (oldAttrs: {
                  subPackages = oldAttrs.subPackages ++ [ "cmd/nginx-auth" ];
                  postInstall = ''
                    wrapProgram $out/bin/tailscaled --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.iproute2 pkgs.iptables ]}
                    wrapProgram $out/bin/tailscale --suffix PATH : ${pkgs.lib.makeBinPath [ pkgs.procps ]}
                    wrapProgram $out/bin/nginx-auth --suffix PATH : ${pkgs.lib.makeBinPath [ pkgs.procps ]}
                    sed -i -e "s#/usr/sbin#$out/bin#" -e "/^EnvironmentFile/d" ./cmd/tailscaled/tailscaled.service
                    sed -i -e "s#/usr/sbin/tailscale.nginx-auth#$out/bin/nginx-auth#" ./cmd/nginx-auth/tailscale.nginx-auth.service
                    sed -i -e "s#/var/run#/run#" ./cmd/nginx-auth/tailscale.nginx-auth.socket
                    install -D -m0444 -t $out/lib/systemd/system ./cmd/tailscaled/tailscaled.service
                    install -D -m0444 -t $out/lib/systemd/system ./cmd/nginx-auth/tailscale.nginx-auth.service
                    install -D -m0444 -t $out/lib/systemd/system ./cmd/nginx-auth/tailscale.nginx-auth.socket
                  '';
                });
              })
            ];
          })
          ./raspberry/nixos.nix
          ./raspberry/hardware-configuration.nix
        ];
      };
      # deploy.nodes.rasp-nixos = {
      #   hostname = "home-lab";
      #   sshUser = "rasp";
      #   sudo = "doas -u";
      #   sshOpts = [ ];
      #   magicRollback = true;
      #   autoRollback = true;
      #   fastConnection = false;
      #   profiles.system = {
      #     user = "root";
      #     path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.rasp-nixos;
      #   };
      # };
      # checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      templates = {
        python = {
          path = ./templates/python-mach-nix;
          description = "A white python mach-nix project";
        };
      };
    };
}


