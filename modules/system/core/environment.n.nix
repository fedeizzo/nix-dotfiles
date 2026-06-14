{
  flake.modules.nixos.environment = { pkgs, ... }: {
    programs = {
      bash = {
        completion.enable = true;
        enableLsColors = true;
      };
      nix-ld.enable = true;
    };

    environment = {
      shells = [ pkgs.bash ];
      variables = {
        "EDITOR" = "vim";
        "VISUAL" = "vim";
      };
      shellAliases = {
        "cp" = "cp --reflink=auto -i";
        "SS" = "systemctl";
      };
      systemPackages = with pkgs; [
        vim
        curl
        bc
        killall
        neofetch
        wget
        git
        highlight
      ];
    };

    virtualisation.oci-containers.backend = "docker";
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;
      };
      podman.enable = false;
    };
  };
}
