{ pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = false;
      character = {
        success_symbol = "[λ](green)";
        vicmd_symbol = "[V](green)";
        error_symbol = "[✖](red)";
      };
      package.disabled = true;
      directory.truncation_length = 8;
      cmd_duration = {
        min_time = 20000;
        format = "  [$duration](bold yellow)";
      };
    };
  };
  programs.fish = {
    enable = true;
    shellAliases = {
      ls = "exa --icons --sort=type";
      ll = "exa -l --icons --sort=type";
      lll = "exa -l --icons --sort=type | less";
      lla = "exa -la --icons --sort=type";
      llt = "exa -T --icons --sort=type";
      vi = "nvim";
      SS = "systemctl";
      it = "setxkbmap it && xmodmap $HOME/.Xmodmap.back";
      us = "setxkbmap -layout us -variant altgr-intl && xmodmap $HOME/.Xmodmap.back";
      streamlink = "streamlink -p 'swayhide vlc'";
      vlc = "swayhide vlc";
      zathura = "swayhide zathura";
      llpp = "swayhide llpp";
      imv = "swayhide imv";
      v = "nvim";
      open = "xdg-open";
      cat = "bat";
      gs = "git status";
      ga = "git add -A";
      gc = "git commit -m";
      gp = "git push";
      noblackscreen = "xset s off -dpms";
      find = "fd";
      lf = "lfrun";
      gotop = "gotop -l kitchensink";
      docker_clean_images = "docker rmi (docker images -a --filter=dangling=true -q)";
      docker_clean_ps = "docker rm (docker ps --filter=status=exited --filter=status=created -q)";
      scp = "rsync -Pavzhe ssh";
      hg = "kitty +kitten hyperlinked_grep";
      colorpicker = "grim -g (slurp -p) -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:-";
      lz = "lazygit";
      er = "systemctl --user restart emacs.service";
    };
    functions = {
      dt = {
        body = ''
          set oldDir (pwd)
          find \
            --absolute-path \
            --full-path $HOME/nix-dotfiles/nixpkgs/ \
            --type 'file' | \
          fzf \
            --preview 'bat \
            --style=numbers \
            --color=always \
            --line-range :500 {}' | \
          read file
          and cd (dirname $file)
          and $EDITOR $file
          and cd $oldDir
        '';
        description = "Search and edit dotfiles";
      };
      up = {
        body = ''
          set oldDir (pwd)
          cd $HOME/nix-dotfiles/nixpkgs/
          ./install.sh
          cd $oldDir
        '';
        description = "Update home-manager config";
      };
      take = {
        argumentNames = "dir";
        body = ''
          mkdir -p $dir
          cd $dir
        '';
      };
      encrypt = {
        argumentNames = "file";
        body = ''
          gpg --encrypt --armor -r 506A41A0BF594872 -o $file.enc $file
        '';
      };
      decrypt = {
        argumentNames = "file";
        body = ''
          set output (echo $file | rev | cut -c5- | rev)
          gpg --output $output --decrypt $file
        '';
      };
      vterm_printf = {
        body = ''
          if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
              # tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
          else if string match -q -- "screen*" "$TERM"
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$argv"
          else
              printf "\e]%s\e\\" "$argv"
          end
        '';
        description = "Comunication channel for emacs-vterm";
      };
      flakify = {
        description = "Use a flake template";
        argumentNames = "name";
        body = ''
          if ! test -e ./flake.nix
            nix flake init -t fedeizzo#$name
          end
          if ! test -e ./.envrc
            echo "use flake" > .envrc
          end
          direnv allow
        '';
      };
    };
    shellAbbrs = {
      "..." = "cd ../..";
      "...." = "cd ../../..";
      "....." = "cd ../../../..";
      "ssh" = "TERM=xterm-256color ssh";
      "gh-md-toc" = "gh-md-toc --no-backup";
    };
    interactiveShellInit = ''
      eval (starship init fish)
      fish_vi_key_bindings
      set -U __done_min_cmd_duration 120000
      set fish_color_command A3BE8C
      set fish_greeting
    '';
    shellInit = ''
      set PATH $PATH ( find $HOME/.sources/ -type d -printf ":%p" )
      set PATH $PATH /home/fedeizzo/.nimble/bin
      set EDITOR "nvim"
      set TERMINAL "kitty"
      set PIPENV_CACHE_DIR "$XDG_CACHE_HOME"/pipenv
      set WPM_COUNTER 0
      set NNN_PLUG 't:treeview'
      set XDG_CONFIG_HOME "$HOME/.config"
      set XDG_CACHE_HOME "$HOME/.cache"
      set XDG_DATA_HOME "$HOME/.local/share"
      set XDG_DATA_DIRS "$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:/home/fedeizzo/.local/share/flatpak/exports/share"
      set CARGO_HOME "$XDG_DATA_HOME"/cargo
      set DOCKER_CONFIG "$XDG_CONFIG_HOME"/docker
      set GRADLE_USER_HOME "$XDG_DATA_HOME"/gradle
      set GRIPHOME "$XDG_CONFIG_HOME/grip"
      set GTK_RC_FILES "$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
      set GTK2_RC_FILES "$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
      set ICEAUTHORITY "$XDG_CACHE_HOME"/ICEauthority
      set IPYTHONDIR "$XDG_CONFIG_HOME"/jupyter
      set JUPYTER_CONFIG_DIR "$XDG_CONFIG_HOME"/jupyter
      set _JAVA_OPTIONS -Djava.util.prefs.userRoot "$XDG_CONFIG_HOME"/java
      set LESSKEY "$XDG_CONFIG_HOME"/less/lesskey
      set LESSHISTFILE "$XDG_CACHE_HOME"/less/history
      set MYSQL_HISTFILE "$XDG_DATA_HOME"/mysql_history
      set NODE_REPL_HISTORY "$XDG_DATA_HOME"/node_repl_history
      set NPM_CONFIG_USERCONFIG $XDG_CONFIG_HOME/npm/npmrc
      set NVM_DIR "$XDG_DATA_HOME"/nvm
      set PSQLRC "$XDG_CONFIG_HOME/pg/psqlrc"
      set PSQL_HISTORY "$XDG_CACHE_HOME/pg/psql_history"
      set PGPASSFILE "$XDG_CONFIG_HOME/pg/pgpass"
      set PGSERVICEFILE "$XDG_CONFIG_HOME/pg/pg_service.conf"
      set PYLINTHOME "$XDG_CACHE_HOME"/pylint
      set TASKDATA "$XDG_DATA_HOME"/task
      set TASKRC "$XDG_CONFIG_HOME"/task/taskrc
      set CABAL_DIR "XDG_DATA_HOME"/.cabal
      set fbk_login_name "fizzo@fbk.eu"
      set diclub_user_name "fizzo"
      set -x SDL_VIDEODRIVER "wayland"
      set -x _JAVA_AWT_WM_NONREPARENTING 1
      set -x QT_QPA_PLATFORM "wayland"
      set -x XDG_CURRENT_DESKTOP "sway"
      set -x XDG_SESSION_DESKTOP "sway"
      set -x MOZ_ENABLE_WAYLAND "1"
      set -x XDG_CURRENT_DESKTOP "Unity"
      set -x XDG_SESSION_TYPE "wayland"
      set -x GTK_USE_PORTAL 0
    '';
    plugins = [
      {
        name = "done";
        src = pkgs.fetchFromGitHub {
          owner = "franciscolourenco";
          repo = "done";
          rev = "1.15.0";
          sha256 = "1i7k59kjik41b7mww6d1qbi66vswplmvjdscinyf60irbrsbc5bv";
        };
      }
      {
        name = "bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "f969c618301163273d0a03d002614d9a81952c1e";
          sha256 = "1r3d4wgdylnc857j08lbdscqbm9lxbm1wqzbkqz1jf8bgq2rvk03";
        };
      }
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "97ca1fd1b281f5f240e7adb90d0a28f9eb4567db";
          sha256 = "sha256-VIeRzaA/Dg0mpCjMB9rNXmIhNGzzYCxgkFUXNUOyJ50=";
        };
      }
      {
        name = "plugin-extract";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-extract";
          rev = "5d05f9f15d3be8437880078171d1e32025b9ad9f";
          sha256 = "0cagh2n5yg8m6ggzhf3kcp714gb8s7blb840kxas0z6366w3qlw4";
        };
      }
    ];
  };
  xdg.configFile."fish/conf.d/sway.fish".source = ../dotfiles/fish/conf.d/sway.fish;
}
