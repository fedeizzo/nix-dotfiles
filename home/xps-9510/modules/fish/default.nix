{ pkgs, sops, ... }:

{
  imports = [
    ../../../common/fish
  ];
  fishPerMachine.aliases = {
    SS = "systemctl";
    # vlc = "swayhide vlc";
    # zathura = "swayhide zathura";
    # llpp = "swayhide llpp";
    # imv = "swayhide imv";
    v = "vim";
    open = "xdg-open";
    lf = "lfrun";
    gotop = "gotop -l kitchensink";
    scp = "rsync -Pavzhe ssh";
    hg = "kitty +kitten hyperlinked_grep";
    colorpicker = "grim -g (slurp -p) -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:-";
    er = "systemctl --user restart emacs.service";
  };
  fishPerMachine.shellInit = ''
    set PATH $PATH ( find $HOME/.sources/ -type d -printf ":%p" )
    set PATH $PATH /home/fedeizzo/.nimble/bin
    set PIPENV_CACHE_DIR "$XDG_CACHE_HOME"/pipenv
    set WPM_COUNTER 0
    set NNN_PLUG 't:treeview'
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
    set -x QT_QPA_PLATFORM "wayland-egl"
    set -x XDG_CURRENT_DESKTOP "Hyprland"
    set -x XDG_SESSION_DESKTOP "Hyprland"
    set -x MOZ_ENABLE_WAYLAND "1"
    set -x XDG_CURRENT_DESKTOP "Hyprland"
    set -x XDG_SESSION_TYPE "wayland"
    set -x GTK_USE_PORTAL 0
  '';
  programs.fish.functions = {
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
    compressZSTD = {
      description = "Compress a directory";
      argumentNames = "directory";
      body = ''
        tar --zstd -cf $directory.tar.zst $directory/
      '';
    };
    extractZSTD = {
      description = "Extract a zstd archive";
      argumentNames = "archive";
      body = ''
        tar --zstd -xf $archive
      '';
    };
  };
  xdg.configFile."fish/conf.d/hyprland.fish".source = ./hyprland.fish;
}
