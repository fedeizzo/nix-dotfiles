{ inputs, pkgs, username, system-overlays, emacs-pkg, pkgs-unstable, ... }:

let
  myFonts = [
    (pkgs.nerdfonts.override {
      fonts = [
        "JetBrainsMono"
      ];
    })
    pkgs.emacs-all-the-icons-fonts
  ];
in
{
  imports = [
    inputs.home-manager.darwinModules.home-manager
    inputs.nh-plus.nixDarwinModules.prebuiltin
    (import ../common/nh { inherit username; isMac = true; })
  ];

  # nix
  system.stateVersion = 4;
  services.nix-daemon.enable = true;
  nix = {
    enable = true;
    settings = {
      experimental-features = "nix-command flakes";
    };
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    optimise.automatic = true;
  };
  nixpkgs.overlays = builtins.attrValues system-overlays;

  # user
  programs.zsh.enable = true;
  fonts.packages = myFonts;
  users.users.${username} = {
    home = "/Users/${username}";
    shell = pkgs.zsh;
  };
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit username inputs emacs-pkg pkgs-unstable;
    };

    users.${username} = import ../../home/macbook-pro;
    backupFileExtension = "backup";
  };
}
