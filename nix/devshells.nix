{ pkgs, config, ... }:

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
        command = ''
          if [[ $USER == "federico.izzo" ]]; then
             darwin-rebuild switch --flake .
             exit 0
          fi

          doas nixos-rebuild switch --flake .
        '';
        category = " System administration";
      }
      {
        help = " Deploy the homelab configuration over ssh.";
        name = "deploy";
        command = "deploy .#rasp-nixos";
        category = " System administration";
        package = pkgs.deploy-rs;
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
        help = " Thinkpad X1 Carbon 6th generation.";
        name = "erase-disk-and-install-x1"; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/x1-carbon.sh;
        category = " System installation";
      }
      {
        help = " Raspberry Pi4 8Gb";
        name = "erase-disk-and-install-raspberry"; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/raspberry.sh;
        category = " System installation";
      }
      {
        help = " Dell XPS 9510";
        name = "erase-disk-and-install-xps"; # long command to prevent accidental use
        command = builtins.readFile ../scripts/installation/xps-9510.sh;
        category = " System installation";
      }
    ];

    packages = with pkgs; [
      sops
    ] ++ config.pre-commit.settings.enabledPackages;

    devshell.startup.pre-commit-hooks.text = config.pre-commit.installationScript;
  };
}
