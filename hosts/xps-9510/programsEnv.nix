{ config, pkgs, nixpkgs-unstable, ... }:

{
  environment.systemPackages = with pkgs; [
    bc
    curl
    killall
    libnotify
    neofetch
    wget
    acpi
    git
    highlight
    lm_sensors
    vim
    virt-manager
    nixpkgs-unstable.qmk
    # to fix xournal problem
    pkgs.gnome.adwaita-icon-theme
    pkgs.shared-mime-info
    seatd
    nixpkgs-unstable.keyd # keyboard mapper
    pinentry-qt
  ];
  programs.sway = {
    enable = true;
    extraOptions = [
      "--unsupported-gpu"
    ];
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export QT_QPA_PLATFORM=wayland
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=Unity
      export XDG_SESSION_TYPE="wayland"
      export GTK_USE_PORTAL=0
      test -f $HOME/.profile && source $HOME/.profile
      export MOZ_ENABLE_WAYLAND=1
      systemctl --user import-environment
    '';
  };
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
    ];
    gtkUsePortal = true;
  };
  virtualisation = {
    docker = {
      enable = true;
      enableNvidia = true;
      enableOnBoot = true;
    };
    libvirtd.enable = true;
  };
  environment.shells = [ pkgs.bash pkgs.fish ];
  environment.pathsToLink = [
    "/share/fish"
    # to fix xournal problem
    "/share/icons"
    "/share/mime"
  ];
  environment.variables = {
    "EDITOR" = "vim";
    "READER" = "zathura";
    "VISUAL" = "vim";
    "CODEEDITOR" = "vim";
    "TERMINAL" = "alacritty";
    "BROWSER" = "firefox";
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
    "v" = "vim";
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
  programs.gnupg.agent.pinentryFlavor = "qt";
  programs.adb.enable = true;
}
