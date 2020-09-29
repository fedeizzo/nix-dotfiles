{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # XORG
    bspwm
    dunst
    libnotify
    sxhkd
    xorg.xbacklight
    picom
    unclutter
    xclip
    xsel
    neofetch
    wget
    curl
    bc
    killall
  
    # OTHER
    acpi
    arandr
    docker
    docker-compose
    git
    highlight
    lm_sensors
  ];

  environment.shells = [ pkgs.bash pkgs.zsh ];
  environment.pathsToLink = [ "/share/zsh" ];

  environment.variables = {
    "EDITOR" = "nvim";
    "READER" = "zathura";
    "VISUAL" = "nvim";
    "CODEEDITOR" = "nvim";
    "TERMINAL" = "alacritty";
    "BROWSER" = "firefox";
    "COLORTERM" = "truecolor";
    # "PAGER" = "less";
    # "LESS" = "-R";
    # "LESSHISTFILE" = "-";
    # "LESS_TERMCAP_mb" = "$'\E[01;31m'";
    # "LESS_TERMCAP_md" = "$'\E[01;31m'";
    # "LESS_TERMCAP_me" = "$'\E[0m'";
    # "LESS_TERMCAP_se" = "$'\E[0m'";
    # "LESS_TERMCAP_so" = "$'\E[01;44;33m'";
    # "LESS_TERMCAP_ue" = "$'\E[0m'";
    # "LESS_TERMCAP_us" = "$'\E[01;32m'";
    # "LESSOPEN" = "|${pkgs.highlight} /bin/highlight -O ansi %s 2>/dev/null";
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

    # exa aliases
    "ls" = "exa --classify --icons";
    "ll" = "exa --classify --group --long --header --git --icons";
    "lll" = "exa --classify --group --long --header --git --icons | less";
    "lla" = "exa --classify --group --long --all --header --git --icons";
    "llt" = "exa --tree --level=3 --icons";

    "SS" = "systemctl";
  };

  fonts = {
    fonts = [
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.joypixels
      pkgs.symbola
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "fira-code" ];
      };
    };
  };
}
