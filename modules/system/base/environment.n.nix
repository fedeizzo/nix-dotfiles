{
  flake.modules.nixos.environment = { pkgs, ... }: {
    programs = {
      bash = {
        completion.enable = true;
        enableLsColors = true;
      };
      nix-ld.enable = true;
    };

    documentation.man.generateCaches = false;

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
        fastfetch
        wget
        git
        highlight
      ];
    };


  };
}
