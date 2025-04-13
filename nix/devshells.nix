{ inputs', pkgs, config, ... }:

{
  devshells.default = {
    motd = ''
      {202}Federico Izzo's flake devshell.{reset}
       $(type -p menu &>/dev/null && menu)
    '';

    commands = [
      {
        help = " Update the system configuration using the current flake and hostname.";
        name = "update";
        command = "nh os switch";
        category = " System administration";
      }
      {
        help = " Delete old generations and clean nix store.";
        name = "clean";
        command = "nh clean all";
        category = " System administration";
        package = inputs'.nh-plus.packages.default;
      }
      {
        help = " Update a flake.nix input.";
        name = "update-input";
        command = ''
          selected_input=$(nix flake metadata | rg '^.───' |  tr -s '\n' '\n' | sed 's/.───//g' | gawk -F':' '{print $1}' | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g" | fzf)

          nix flake update $selected_input
        '';
        category = " System administration";
      }
      {
        help = " Deploy the homelab configuration over ssh.";
        name = "deploy-homelab";
        command = "deploy --skip-checks .#homelab";
        category = " System administration";
      }
      {
        help = " Deploy the homelab configuration over ssh without any rollback strategy.";
        name = "deploy-homelab-unsafe";
        command = "deploy --auto-rollback false -s --rollback-succeeded false --magic-rollback false .#homelab";
        category = " System administration";
      }
      {
        help = " Print the current plasma configuration.";
        name = "plasma-manager";
        command = "nix run github:nix-community/plasma-manager";
        category = " System administration";
      }
      {
        help = " Refresh the devshell.";
        name = "refresh";
        command = "touch flake.nix";
        category = " System administration";
      }
      {
        help = " Edit secrets.";
        name = "secrets";
        command = builtins.readFile ../scripts/edit_secrets.sh;
        category = " System administration";
      }
      {
        help = " Generate topology image";
        name = "topology";
        command = ''
          nix build .#topology.x86_64-linux.config.output
        '';
        category = " Repository administration";
      }
      {
        help = " Thinkpad X1 Nano 6th generation.";
        name = "erase-disk-and-install-x1."; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/x1-nano.sh;
        category = " System installation";
      }
      {
        help = " Raspberry Pi4 8Gb.";
        name = "erase-disk-and-install-raspberry"; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/raspberry.sh;
        category = " System installation";
      }
      {
        help = " Dell XPS 9510.";
        name = "erase-disk-and-install-xps"; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/xps-9510.sh;
        category = " System installation";
      }
    ];

    packages = with pkgs; [
      sops
      deploy-rs
    ] ++ config.pre-commit.settings.enabledPackages;

    devshell.startup.pre-commit-hooks.text = config.pre-commit.installationScript;
  };
}
