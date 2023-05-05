{ pkgs, config, lib, ... }:

let
  cfg = config.fishPerMachine;
  commonAliases = {
    ls = "exa --icons --sort=type";
    ll = "exa -l --icons --sort=type";
    lll = "exa -l --icons --sort=type | less";
    lla = "exa -la --icons --sort=type";
    llt = "exa -T --icons --sort=type";
    cat = "bat";
    gs = "git status";
    ga = "git add -A";
    gc = "git commit -m";
    gp = "git push";
    find = "fd";
  };
  commonShellInit = ''
    set EDITOR "vim"
    set TERMINAL "kitty"
    set XDG_CONFIG_HOME "$HOME/.config"
    set XDG_CACHE_HOME "$HOME/.cache"
    set XDG_DATA_HOME "$HOME/.local/share"
  '';
in
{
  options.fishPerMachine = {
    aliases = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
    shellInit = lib.mkOption {
      type = lib.types.str;
      default = "";
    };
  };
  config = {
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
      shellAliases = lib.mergeAttrs commonAliases cfg.aliases;
      shellAbbrs = {
        "..." = "cd ../..";
        "...." = "cd ../../..";
        "....." = "cd ../../../..";
        "ssh" = "TERM=xterm-256color ssh";
      };
      interactiveShellInit = ''
        eval (${pkgs.starship}/bin/starship init fish)
        set -U __done_min_cmd_duration 120000
        set fish_color_command A3BE8C
        set fish_greeting
        source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
        source ~/.dd-setup.fish
      '';
      shellInit = commonShellInit + "\n" + cfg.shellInit;
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
        {
          name = "zfz";
          src = pkgs.fetchFromGitHub {
            owner = "PatrickF1";
            repo = "fzf.fish";
            rev = "63c8f8e65761295da51029c5b6c9e601571837a1";
            sha256 = "sha256-i9FcuQdmNlJnMWQp7myF3N0tMD/2I0CaMs/PlD8o1gw=";
          };
        }
      ];
    };
  };
}
