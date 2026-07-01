{
  flake.modules.homeManager.zsh = { pkgs, lib, config, ... }: {
    programs = {
      zsh = {
        enable = true;
        autosuggestion.enable = true;
        enableCompletion = true;
        enableVteIntegration = true;
        syntaxHighlighting.enable = true;
        dotDir = "${config.xdg.configHome}/zsh";
        history = {
          # extended = true;
          ignoreSpace = true;
          save = 10000;
          size = 10000;
        };
        historySubstringSearch.enable = true;
        initContent = lib.mkAfter ''
          function compressZSTD() {
            tar --zstd -cf "$1.tar.zst" "$1/"
          }

          function extractZSTD() {
            tar --zstd -xf "$1"
          }
        '';
        envExtra = lib.mkAfter ''
          export LANG="en_US.UTF-8"
          export EDITOR="vim"
          export XDG_CONFIG_HOME="$HOME/.config"
          export XDG_CACHE_HOME="$HOME/.cache"
          export XDG_DATA_HOME="$HOME/.local/share"
          export TERMINAL="ghostty"
          export PATH="$PATH:/home/fedeizzo/.nimble/bin"
          export PIPENV_CACHE_DIR="$XDG_CACHE_HOME/pipenv"
          export XDG_DATA_DIRS="$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:/home/fedeizzo/.local/share/flatpak/exports/share"
          export CARGO_HOME="$XDG_DATA_HOME/cargo"
          export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
          export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
          export GRIPHOME="$XDG_CONFIG_HOME/grip"
          export GTK_RC_FILES="$XDG_CONFIG_HOME/gtk-1.0/gtkrc"
          export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
          export ICEAUTHORITY="$XDG_CACHE_HOME/ICEauthority"
          export IPYTHONDIR="$XDG_CONFIG_HOME/jupyter"
          export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
          export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=\"$XDG_CONFIG_HOME/java\""
          export LESSKEY="$XDG_CONFIG_HOME/less/lesskey"
          export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
          export MYSQL_HISTFILE="$XDG_DATA_HOME/mysql_history"
          export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
          export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
          export NVM_DIR="$XDG_DATA_HOME/nvm"
          export PSQLRC="$XDG_CONFIG_HOME/pg/psqlrc"
          export PSQL_HISTORY="$XDG_CACHE_HOME/pg/psql_history"
          export PGPASSFILE="$XDG_CONFIG_HOME/pg/pgpass"
          export PGSERVICEFILE="$XDG_CONFIG_HOME/pg/pg_service.conf"
          export PYLINTHOME="$XDG_CACHE_HOME/pylint"
          export CABAL_DIR="$XDG_DATA_HOME/.cabal"
          export _JAVA_AWT_WM_NONREPARENTING=1
          export GTK_USE_PORTAL=0
        '';
        shellAliases = {
          ls = "eza --icons --sort=type";
          ll = "eza -l --icons --sort=type";
          lll = "eza -l --icons --sort=type | less";
          lla = "eza -la --icons --sort=type";
          llt = "eza -T --icons --sort=type";
          cat = "bat";
          gs = "git status";
          ga = "git add -A";
          gc = "git commit -m";
          gp = "git push";
          find = "fd";
          open = "xdg-open";
          scp = "rsync -Pavzh -e \"ssh\"";
          hg = "rg";
          colorpicker = "grim -g (slurp -p) -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:-";
          "..." = "cd ../..";
          "...." = "cd ../../..";
          "....." = "cd ../../../..";
          "ssh" = "TERM=xterm-256color ssh";
        };
        oh-my-zsh = {
          enable = true;
          plugins = [
            "git"
            "aliases"
            "eza"
            "gh"
            "jj"
            "extract"
            "z"
          ];
        };
        plugins = [
          {
            name = "fzf-tab";
            src = pkgs.fetchFromGitHub {
              owner = "Aloxaf";
              repo = "fzf-tab";
              rev = "v1.2.0";
              sha256 = "sha256-q26XVS/LcyZPRqDNwKKA9exgBByE0muyuNb0Bbar2lY=";
            };
          }
        ];
      };

      fzf = {
        enable = true;
        enableZshIntegration = true;
      };

      zoxide = {
        enable = true;
        enableZshIntegration = true;
        options = [ ];
      };
    };
  };

  flake.modules.nixos.zsh = { ... }: {
    programs.zsh.enable = true;
  };
}
