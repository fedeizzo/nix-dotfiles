{ pkgs, ... }:

{
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
      streamlink = "streamlink -p 'devour vlc'";
      vlc = "devour vlc";
      zathura = "devour zathura";
      llpp = "devour llpp";
      sxiv = "devour sxiv";
      v = "nvim";
      open = "xdg-open";
      cat = "bat";
      gt = "gitui";
      gs = "git status";
      ga = "git add -A";
      gc = "git commit -m";
      gp = "git push";
      noblackscreen = "xset s off -dpms";
      find = "fd";
    };
    functions = {
      dt = {
        body = ''
          set oldDir (pwd)
          find \
            --absolute-path \
            --full-path $HOME/nixOs-config/nixpkgs/ \
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
          cd $HOME/nixOs-config/nixpkgs/
          ./install.sh
          cd $oldDir
        '';
        description = "Update home-manager config";
      };
      take = {
        argumentNames="dir";
        body = ''
          mkdir -p $dir
          cd $dir
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
    promptInit = ''
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
      set TERMINAL "st"
      set PIPENV_CACHE_DIR "$XDG_CACHE_HOME"/pipenv
      set WPM_COUNTER 0
      set NNN_PLUG 't:treeview'
      set XDG_CONFIG_HOME "$HOME/.config"
      set XDG_CACHE_HOME "$HOME/.cache"
      set XDG_DATA_HOME "$HOME/.local/share"
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
}
