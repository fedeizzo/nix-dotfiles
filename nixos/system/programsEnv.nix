{ config, pkgs, ... }:

{
  #################################
  # PROGRAMS and ENV
  #################################
  environment.systemPackages = with pkgs; [
    bc
    curl
    dunst
    haskellPackages.xmobar
    killall
    libnotify
    neofetch
    # picom
    unclutter
    wget
    xclip
    xorg.xbacklight
    xsel
    acpi
    arandr
    docker
    docker-compose
    podman
    podman-compose
    git
    highlight
    lm_sensors
    vim
    xorg.xinit
    virt-manager
  ];
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      enableNvidia = true;
    };
    podman.enable = true;
    libvirtd.enable = true;
  };
  environment.shells = [ pkgs.bash pkgs.fish ];
  environment.pathsToLink = [ "/share/fish" ];
  environment.variables = {
    "EDITOR" = "nvim";
    "READER" = "zathura";
    "VISUAL" = "nvim";
    "CODEEDITOR" = "nvim";
    "TERMINAL" = "alacritty";
    "BROWSER" = "qutebrowser";
    "COLORTERM" = "truecolor";
  };
  environment.shellAliases = {
    # cp optimized for btrfs
    "cp" = "cp --reflink=auto -i";
    # some useful aliases
    "grep" = "grep --color=auto";
    "ip" = "ip -c ";
    ":q" = "exit";
    "mv" = "mv -i";
    "open" = "xdg-open";
    # editor aliases
    "v" = "nvim";
    "SS" = "systemctl";
  };
  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };
  programs.fish.enable = true;
  programs.light.enable = true;
  programs.ssh.askPassword = "";
  programs.ccache.enable = true;
  programs.gnupg.agent.enable = true;
}