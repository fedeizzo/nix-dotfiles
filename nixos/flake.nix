{
  description = "My personal NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = { self, nixpkgs, nixos-hardware, ... }@inputs:
    {
      # by default the configuration used for nixos-rebuild switch
      # is matched with the current hostname
      nixosConfigurations.fedeizzo-nixos = nixpkgs.lib.nixosSystem {
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
              ];
            })
            ./system/boot.nix
            ./system/hardware.nix
            ./system/hardware-configurations/ext4.nix
            ./system/keymapTimeFont.nix
            ./system/pipewire.nix
            ./system/networking.nix
            ./system/nixNixOS.nix
            ./system/nvidia.nix
            ./system/programsEnv.nix
            ./system/security.nix
            ./system/services.nix
            ./system/user.nix
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
    };
}


