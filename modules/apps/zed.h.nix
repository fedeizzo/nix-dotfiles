{
  flake.modules.homeManager.zed = { pkgs, lib, pkgs-unstable ? pkgs, ... }: {
    programs.zed-editor = {
      enable = true;
      package = pkgs-unstable.zed-editor;
      extensions = [
        "basher"
        "codebook"
        "docker-compose"
        "dockerfile"
        "env"
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
        "sqruff"
        "svelte"
        "terraform"
        "toml"
        "tree-sitter-query"
        "proto"
        "zed-react-ts-snippets"
        "rust"
        "rust-snippets"
        "catppuccin"
        "bearded-icon-theme"
      ];
      userSettings = {
        icon_theme = "Catppuccin Macchiato";
        theme = {
          mode = "system";
          dark = "Catppuccin Macchiato";
          light = "Catppuccin Latte";
        };
        ui_font_size = 13;
        buffer_font_size = 13;
        project_panel = {
          dock = "left";
        };
        active_pane_modifiers = {
          inactive_opacity = 0.9;
          border_size = 1;
        };
        telemetry = { diagnostics = false; metrics = false; };
        features = { copilot = false; };
        format_on_save = "on";
        autosave = { after_delay = { milliseconds = 1000; }; };
        auto_update = false;
        load_direnv = "shell_hook";
        current_line_highlight = "gutter";
        tabs = { file_icons = true; git_status = true; };
        soft_wrap = "editor_width";
        vim_mode = true;
        vim = {
          use_smartcase_find = true;
          use_multiline_find = true;
          toggle_relative_line_numbers = true;
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
        lsp = {
          gopls = {
            initialization_options = {
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
              analyses = { shadow = true; };
            };
          };
          ruff = { initialization_options = { settings = { lineLength = 120; }; }; };
          nil = {
            binary.path = lib.getExe pkgs-unstable.nil;
            initialization_options = { formatting.command = [ "nixpkgs-fmt" ]; };
          };
          vtsls = { settings = { typescript = { tsserver = { maxTsServerMemory = 16184; }; }; }; };
        };
        languages = {
          Python = {
            language_servers = [ "pylsp" "ruff" "python-refactoring" "!pyright" "..." ];
            format_on_save = "on";
            formatter = [
              { language_server = { name = "ruff"; }; }
              { code_action = "source.fixAll.ruff"; }
              { code_action = "source.organizeImports.ruff"; }
            ];
          };
          Nix = { language_servers = [ "nil" "!nixd" "..." ]; };
        };
        # assistant = { version = "2"; default_model = { provider = "ollama"; model = "qwen3.5:35b-a3b"; }; };
        # agent = {
        #   default_model = { provider = "ollama"; model = "qwen3.5:35b-a3b"; };
        # inline_assistant_model = { provider = "ollama"; model = "qwen3.5:35b-a3b"; };
        # };
        # language_models = {
        #   ollama = {
        #     api_url = "https://ollama.fedeizzo.dev";
        #     available_models = [
        #       {
        #         name = "qwen3.5:35b-a3b";
        #         display_name = "qwen3.5:35b-a3b";
        #         max_tokens = 202752;
        #         keep_alive = "15m";
        #         supports_tools = true;
        #         supports_thinking = false;
        #         supports_images = false;
        #       }
        #     ];
        #   };
        # };

      };
    };
  };
}
