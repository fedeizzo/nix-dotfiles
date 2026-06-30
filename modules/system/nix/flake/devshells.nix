{ inputs, ... }:

{
  flake-file.inputs.devshell.url = "github:numtide/devshell";

  imports = [
    inputs.devshell.flakeModule
  ];

  perSystem = { config, pkgs, ... }: {
    devshells.default = {
      motd = ''
        {202}Federico Izzo's flake devshell.{reset}
         $(type -p menu &>/dev/null && menu)
      '';

      commands = [
        {
          help = "🔄 Switch and update the current system configuration using nh.";
          name = "update";
          command = "doas nh os switch -R";
          category = "🔧 System administration";
        }
        {
          help = "🧹 Garbage collect old NixOS generations and clean the store.";
          name = "clean";
          command = "doas nh clean all -R";
          category = "🔧 System administration";
          package = inputs.nh.packages.${pkgs.system}.default;
        }
        {
          help = "📥 Interactively select and update a specific flake.nix input.";
          name = "update-input";
          command = ''
            selected_input=$(nix flake metadata | rg '^.───' |  tr -s '\n' '\n' | sed 's/.*───//g' | gawk -F':' '{print $1}' | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g" | fzf)

            nix flake update $selected_input
            nix run .#write-flake
          '';
          category = "🔧 System administration";
        }
        {
          help = "🚀 Deploy the configuration to the homelab server over SSH.";
          name = "deploy-homelab";
          command = "deploy --skip-checks .#homelab";
          category = "📡 Remote administration";
        }
        {
          help = "👢 Deploy the homelab configuration and activate it on the next boot.";
          name = "deploy-homelab-boot";
          command = "deploy --skip-checks --boot .#homelab";
          category = "📡 Remote administration";
        }
        {
          help = "⚠️ Force deploy the homelab configuration without rollback safety limits.";
          name = "deploy-homelab-unsafe";
          command = "deploy --auto-rollback false -s --rollback-succeeded false --magic-rollback false .#homelab";
          category = "📡 Remote administration";
        }
        {
          help = "♻️ Regenerate flake.nix via flake-file and reload the devshell.";
          name = "refresh";
          command = "nix run .#write-flake && touch flake.nix";
          category = "🔧 System administration";
        }
        {
          help = "🔐 Edit SOPS encrypted secrets file securely.";
          name = "secrets";
          command = builtins.readFile ../../../../scripts/edit_secrets.sh;
          category = "🔧 System administration";
        }
        {
          help = "🗺️ Generate a visual SVG map of the network topology.";
          name = "topology";
          command = ''
            nix build .#topology.x86_64-linux.config.output
          '';
          category = "📁 Repository administration";
        }
        {
          help = "💻 CAUTION: Erase disk and perform a clean install on the Thinkpad X1 Nano.";
          name = "erase-disk-and-install-x1."; # long command to prevent accidental use
          command = builtins.readFile ../../../../scripts/installation/x1-nano.sh;
          category = "💿 System installation";
        }
        {
          help = "🍓 CAUTION: Erase disk and perform a clean install on the Raspberry Pi 4.";
          name = "erase-disk-and-install-raspberry"; # long command to prevent accidental use
          command = builtins.readFile ../../../../scripts/installation/raspberry.sh;
          category = "💿 System installation";
        }
        {
          help = "💾 Build an ISO SD Card image for the Raspberry Pi 4.";
          name = "build-raspberry-iso-sd-card"; # long command to prevent accidental use
          command = builtins.readFile ../../../../scripts/build_raspberry_iso_sd_card.sh;
          category = "💿 System installation";
        }
        {
          help = "💻 CAUTION: Erase disk and perform a clean install on the Dell XPS 9510.";
          name = "erase-disk-and-install-xps"; # long command to prevent accidental use
          command = builtins.readFile ../../../../scripts/installation/xps-9510.sh;
          category = "💿 System installation";
        }
        {
          help = "🐍 Run the python validation script to check dendritic module aspects.";
          name = "validate-aspects";
          command = "python3 scripts/validate-aspects.py \"$@\"";
          category = "📁 Repository administration";
        }
        {
          help = "🌳 List all evaluated dendritic modules mapped to a specific hostname.";
          name = "list-modules";
          command = "python3 scripts/list-enabled-modules.py \"$@\"";
          category = "📁 Repository administration";
        }
        {
          help = "✍️  Regenerate flake.nix from module options via flake-file.";
          name = "gen-flake";
          command = "nix run .#write-flake";
          category = "📁 Repository administration";
        }
      ];

      packages = with pkgs; [
        sops
        deploy-rs
        mdbook
        python3
        just
        
        go
        gopls
        go-mockery
        gotestsum
        gotestdox
      ] ++ config.pre-commit.settings.enabledPackages;

      devshell.startup.pre-commit-hooks.text = config.pre-commit.installationScript;
    };
  };
}
