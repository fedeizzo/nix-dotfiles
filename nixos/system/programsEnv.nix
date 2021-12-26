{ config, pkgs, nixpkgs-unstable, ... }:

{
  #################################
  # PROGRAMS and ENV
  #################################
  nixpkgs.overlays = [
    (import ../pkgs)
    (self: super: {
      podman-compose = super.podman-compose.overrideAttrs (old: {
        version = "1.0.2";
        src = super.fetchFromGitHub {
          repo = "podman-compose";
          owner = "containers";
          rev = "v1.0.2";
          sha256 = "sha256-VTy9sE5lUny4ruzyr8M3DVvv6F4ZgpIXiVOhn+sjV78=";
        };
        propagatedBuildInputs = [
          super.python3Packages.pyyaml
          super.python3Packages.python-dotenv
        ];
      });
    })
  ];
  environment.systemPackages = with pkgs; [
    bc
    curl
    dunst
    haskellPackages.xmobar
    killall
    libnotify
    neofetch
    wget
    xclip
    xorg.xbacklight
    autorandr
    xsel
    acpi
    arandr
    # docker
    # docker-compose
    podman
    podman-compose
    git
    highlight
    lm_sensors
    vim
    xorg.xinit
    virt-manager
    nixpkgs-unstable.qmk
  ];
  programs.sway = {
    enable = true;
    extraOptions = [
      "--my-next-gpu-wont-be-nvidia"
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
    # docker = {
    #   enable = true;
    #   enableOnBoot = true;
    #   enableNvidia = false;
    # };
    podman = {
      enable = true;
      enableNvidia = true;
    };
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
  programs.gnupg.agent.pinentryFlavor = "qt";
}
