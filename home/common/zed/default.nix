{ config
, lib
, pkgs-unstable
, ...
}:

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
        "git-firefly"
        "golangci-lint"
        "go-snippets"
        "sqlc-snippets"
        "gosum"
        "helm"
        "html"
        "hurl"
        "just"
        "nix"
        "pylsp"
        "python-refactoring"
        "python-snippet"
        "ruff"
        "sql"
        "sqruff" # sql lsp support
        "svelte"
        "terraform"
        "toml"
        "tree-sitter-query"
        "proto"
        "zed-react-ts-snippets" # react snippets
        "rust"
        "rust-snippets"

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
          copilot = false;
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
        diagnostic = {
          button = true;
          include_warnings = true;
          inline = {
            enabled = true;
            update_debounce_ms = 150;
            padding = 4;
            min_column = 0;
            max_severity = "all";
          };
          cargo = null;
        };

        # lsp
        lsp = {
          gopls = {
            binary.path_lookup = true;
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
          rust-analyzer = {
            binary = {
              path_lookup = true;
            };
          };
          package-version-server = {
            binary = {
              path_lookup = true;
            };
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
          vtsls = {
            settings = {
              typescript = {
                tsserver = {
                  maxTsServerMemory = 16184;
                };
              };
            };
          };
        };

        # languages
        languages = {
          Python = {
            language_servers = [
              "pylsp"
              "ruff"
              "python-refactoring"
              "!pyright"
              "..."
            ];
            format_on_save = "on";
            formatter = [
              {
                language_server = {
                  name = "ruff";
                };
              }
              {
                code_action = "source.fixAll.ruff";
              }
              {
                code_action = "source.organizeImports.ruff";
              }
            ];
          };
          Nix = {
            language_servers = [
              "nil"
              "!nixd"
              "..."
            ];
          };
        };

        assistant = {
          version = "2";
          default_model = {
            provider = "lmstudio";
            model = "mistralai/devstral-small-2-2512";
          };
        };

        agent = {
          default_model = {
            provider = "lmstudio";
            model = "mistralai/devstral-small-2-2512";
          };
          inline_assistant_model = {
            provider = "lmstudio";
            model = "mistralai/devstral-small-2-2512";
          };
        };

        language_models = {
          lmstudio = {
            api_url = "https://llm.fedeizzo.dev/v1";
            available_models = [
              # {
              #   name = "unsloth/qwen3-coder-30b-a3b-instruct";
              #   display_name = "Qwen 3 30b (Local)";
              #   max_tokens = 32768;
              #   supports_tool_calls = true;
              #   supports_images = false;
              # }
              {
                name = "mistralai/devstral-small-2-2512";
                display_name = "Devstral";
                max_tokens = 262144;
                supports_tool_calls = true;
                supports_images = false;
              }
              # {
              #   name = "qwen/qwen3-vl-8b";
              #   display_name = "Qwen 3 VL (Local)";
              #   max_tokens = 32768;
              #   supports_tool_calls = true;
              #   supports_images = false;
              # }
              # {
              #   name = "qwen/qwen3-coder-30b";
              #   display_name = "Qwen coder 30 (Local)";
              #   max_tokens = 32768;
              #   supports_tool_calls = true;
              #   supports_images = false;
              # }
              # {
              #   name = "mistralai/ministral-3-3b";
              #   display_name = "Mistral 3 3b";
              #   max_tokens = 262144;
              #   supports_tool_calls = true;
              #   supports_images = false;
              # }
            ];
          };
        };

      };
    };
    xdg.configFile."zed/tasks.json".text = (builtins.toJSON cfg.tasks);
  };
}
