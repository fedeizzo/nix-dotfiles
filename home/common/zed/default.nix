{ config, lib, pkgs-unstable, ... }:

with lib;
let
  cfg = config.zed-fedeizzo;
  jsonFormat = pkgs-unstable.formats.json { };

in
{
  options.zed-fedeizzo = {
    enable = mkEnableOption "fedeizzo's zed module";

    gopls = {
      buildFlags = mkOption {
        type = with types; listOf str;
        default = [ ];
      };
      directoryFilters = mkOption {
        type = with types; listOf str;
        default = [ ];
      };
    };

    tasks = mkOption {
      type = jsonFormat.type;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    programs.zed-editor = {
      enable = true;
      package = pkgs-unstable.zed-editor;
      extensions = [
        "basher"
        "codebook" # spell check
        "docker-compose"
        "dockerfile"
        "env" # .env and conf file
        "golangci-lint"
        "gosum"
        "helm"
        "html"
        "hurl"
        "just"
        "nix"
        "pylsp"
        "python-refactoring"
        "ruff"
        "sql"
        "sqruff" # sql lsp support
        "svelte"
        "terraform"
        "toml"
        "tree-sitter-query"
        "proto"

        # ui
        "nord"
        "bearded-icon-theme"
      ];

      # extraPackages = with pkgs-unstable; [
      #   gopls
      # ];

      userSettings = {
        # UI
        icon_theme = "Bearded Icon Theme";
        ui_font_size = 13;
        buffer_font_size = 13;
        theme = {
          mode = "dark";
          light = "One Light";
          dark = "One Dark";
        };
        active_pane_modifiers = {
          inactive_opacity = 0.9;
          border_size = 1;
        };

        # telemtry and AI
        telemetry = {
          diagnostics = false;
          metrics = false;
        };
        features = {
          edit_prediction_provider = "copilot";
          copilot = true;
        };
        agent = {
          enabled = false;
          version = "1";
          # version = "2";
          # default_model = {
          #   provider = "copilot_chat";
          #   model = "claude-3.7-sonnet";
          # };
          # editor_model = {
          #   provider = "copilot_chat";
          #   model = "claude-3.7-sonnet";
          # };
        };

        # features
        format_on_save = "on";
        autosave = {
          after_delay = {
            milliseconds = 1000;
          };
        };
        auto_update = false;
        load_direnv = "shell_hook";
        current_line_highlight = "gutter";
        tabs = {
          file_icons = true;
          git_status = true;
        };
        soft_wrap = "editor_width";
        vim_mode = true;
        vim = {
          use_smartcase_find = true;
          use_multiline_find = true;
          toggle_relative_line_numbers = true;
          # default_mode = "helix_normal";
        };

        # lsp
        lsp = {
          gopls = {
            # binary.path = getExe pkgs-unstable.gopls;
            initialization_options = {
              buildFlags = cfg.gopls.buildFlags;
              directoryFilters = cfg.gopls.directoryFilters;
              # local = "github.com/DataDog/dd-go"; # TODO figure this
              codelenses = {
                generate = true;
                regenerate_cgo = true;
                run_govulncheck = false;
                tidy = true;
                upgrade_dependency = true;
                vendor = true;
              };
              hints = {
                assignVariableTypes = true;
                compositeLiteralFields = true;
                compositeLiteralTypes = true;
                constantValues = true;
                functionTypeParameters = true;
                parameterNames = true;
                rangeVariableTypes = true;
              };
              analyses = {
                shadow = true;
              };
            };
          };
          pylsp = {
            binary.path_lookup = true;
          };
          ruff = {
            initialization_options = {
              settings = {
                lineLength = 120;
              };
            };
          };
          nil = {
            binary.path = getExe pkgs-unstable.nil;
            initialization_options = {
              formatting.command = [ "nixpkgs-fmt" ];
            };
          };
        };

        # languages
        languages = {
          Python = {
            language_servers = [ "pylsp" "ruff" "python-refactoring" "!pyright" "..." ];
            format_on_save = "on";
            formatter = [
              {
                language_server = { name = "ruff"; };
              }
              {
                code_actions = {
                  "source.fixAll.ruff" = true;
                  "source.organizeImports.ruff" = true;
                };
              }
            ];
          };
          Nix = {
            language_servers = [ "nil" "!nixd" "..." ];
          };
        };
      };
    };
    xdg.configFile."zed/tasks.json".text = (builtins.toJSON cfg.tasks);
  };
}
