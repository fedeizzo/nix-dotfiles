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
      default = { };
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
          mode = "system";
          light = "One Light";
          dark = "One Dark";
        };
        active_pane_modifiers = {
          inactive_opacity = 0.9;
          border_size = 1;
          magnification = 1;
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
        assistant = {
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

      # userKeymaps = [
      #   {
      #     context = "Editor && vim_mode != helix_normal && vim_operator == none && !VimWaiting";
      #     bindings = {
      #       escape = "vim::SwitchToHelixNormalMode";
      #     };
      #   }
      #   {
      #     context = "vim_mode == helix_normal && !menu";
      #     bindings = {
      #       "0" = "vim::StartOfLine";
      #       "1" = [ "vim::Number" 1 ];
      #       "2" = [ "vim::Number" 2 ];
      #       "3" = [ "vim::Number" 3 ];
      #       "4" = [ "vim::Number" 4 ];
      #       "5" = [ "vim::Number" 5 ];
      #       "6" = [ "vim::Number" 6 ];
      #       "7" = [ "vim::Number" 7 ];
      #       "8" = [ "vim::Number" 8 ];
      #       "9" = [ "vim::Number" 9 ];
      #       space = "vim::Space";
      #       "$" = "vim::EndOfLine";
      #       end = "vim::EndOfLine";
      #       "^" = "vim::FirstNonWhitespace";
      #       _ = "vim::StartOfLineDownward";
      #       "g _" = "vim::EndOfLineDownward";
      #       shift-g = "vim::EndOfDocument";
      #       "{" = "vim::StartOfParagraph";
      #       "}" = "vim::EndOfParagraph";
      #       "(" = "vim::SentenceBackward";
      #       ")" = "vim::SentenceForward";
      #       "|" = "vim::GoToColumn";
      #       "] ]" = "vim::NextSectionStart";
      #       "] [" = "vim::NextSectionEnd";
      #       "[ [" = "vim::PreviousSectionStart";
      #       "[ ]" = "vim::PreviousSectionEnd";
      #       "] m" = "vim::NextMethodStart";
      #       "] M" = "vim::NextMethodEnd";
      #       "[ m" = "vim::PreviousMethodStart";
      #       "[ M" = "vim::PreviousMethodEnd";
      #       "[ *" = "vim::PreviousComment";
      #       "[ /" = "vim::PreviousComment";
      #       "] *" = "vim::NextComment";
      #       "] /" = "vim::NextComment";
      #       w = "vim::NextWordStart";
      #       e = "vim::NextWordEnd";
      #       b = "vim::PreviousWordStart";
      #       shift-w = [ "vim::NextWordStart" { ignore_punctuation = true; } ];
      #       shift-e = [ "vim::NextWordEnd" { ignore_punctuation = true; } ];
      #       shift-b = [ "vim::PreviousWordStart" { ignore_punctuation = true; } ];
      #       "g shift-e" = [ "vim::PreviousWordEnd" { ignore_punctuation = true; } ];
      #       "/" = "vim::Search";
      #       "g /" = "pane::DeploySearch";
      #       "?" = [ "vim::Search" { backwards = true; } ];
      #       "*" = "vim::MoveToNext";
      #       "#" = "vim::MoveToPrevious";
      #       n = "vim::MoveToNextMatch";
      #       shift-n = "vim::MoveToPreviousMatch";
      #       "%" = "vim::Matching";
      #       "] }" = [ "vim::UnmatchedForward" { char = "}"; } ];
      #       "[ {" = [ "vim::UnmatchedBackward" { char = "{"; } ];
      #       "] )" = [ "vim::UnmatchedForward" { char = ")"; } ];
      #       "[ (" = [ "vim::UnmatchedBackward" { char = "("; } ];
      #       f = [ "vim::PushFindForward" { before = false; } ];
      #       t = [ "vim::PushFindForward" { before = true; } ];
      #       shift-f = [ "vim::PushFindBackward" { after = false; } ];
      #       shift-t = [ "vim::PushFindBackward" { after = true; } ];
      #       m = "vim::PushMark";
      #       "'" = [ "vim::PushJump" { line = true; } ];
      #       "`" = [ "vim::PushJump" { line = false; } ];
      #       ";" = "vim::RepeatFind";
      #       "," = "vim::RepeatFindReversed";
      #       ctrl-o = "pane::GoBack";
      #       ctrl-i = "pane::GoForward";
      #       "ctrl-]" = "editor::GoToDefinition";
      #       v = "vim::ToggleVisual";
      #       shift-v = "vim::ToggleVisualLine";
      #       ctrl-v = "vim::ToggleVisualBlock";
      #       ctrl-q = "vim::ToggleVisualBlock";
      #       shift-k = "editor::Hover";
      #       shift-r = "vim::ToggleReplace";
      #       home = "vim::StartOfLine";
      #       ctrl-f = "vim::PageDown";
      #       pagedown = "vim::PageDown";
      #       ctrl-b = "vim::PageUp";
      #       pageup = "vim::PageUp";
      #       ctrl-d = "vim::ScrollDown";
      #       ctrl-u = "vim::ScrollUp";
      #       ctrl-e = "vim::LineDown";
      #       ctrl-y = "vim::LineUp";
      #       "g g" = "vim::StartOfDocument";
      #       "g d" = "editor::GoToDefinition";
      #       "g shift-d" = "editor::GoToDeclaration";
      #       "g shift-i" = "editor::GoToImplementation";
      #       "g x" = "editor::OpenUrl";
      #       "g f" = "editor::OpenFile";
      #       "g shift-l" = "vim::SelectPrevious";
      #       "g >" = [ "editor::SelectNext" { replace_newest = true; } ];
      #       "g <" = [ "editor::SelectPrevious" { replace_newest = true; } ];
      #       "g a" = "editor::SelectAllMatches";
      #       "g shift-s" = "project_symbols::Toggle";
      #       "g ." = "editor::ToggleCodeActions";
      #       "g shift-a" = "editor::FindAllReferences";
      #       "g space" = "editor::OpenExcerpts";
      #       "g *" = [ "vim::MoveToNext" { partial_word = true; } ];
      #       "g #" = [ "vim::MoveToPrevious" { partial_word = true; } ];
      #       "g j" = [ "vim::Down" { display_lines = true; } ];
      #       "g down" = [ "vim::Down" { display_lines = true; } ];
      #       "g k" = [ "vim::Up" { display_lines = true; } ];
      #       "g up" = [ "vim::Up" { display_lines = true; } ];
      #       "g $" = [ "vim::EndOfLine" { display_lines = true; } ];
      #       "g end" = [ "vim::EndOfLine" { display_lines = true; } ];
      #       "g 0" = [ "vim::StartOfLine" { display_lines = true; } ];
      #       "g home" = [ "vim::StartOfLine" { display_lines = true; } ];
      #       "g ^" = [ "vim::FirstNonWhitespace" { display_lines = true; } ];
      #       "g v" = "vim::RestoreVisualSelection";
      #       "g ]" = "editor::GoToDiagnostic";
      #       "g [" = "editor::GoToPreviousDiagnostic";
      #       "g i" = "vim::InsertAtPrevious";
      #       "g ," = "vim::ChangeListNewer";
      #       "g ;" = "vim::ChangeListOlder";
      #       shift-h = "vim::WindowTop";
      #       shift-m = "vim::WindowMiddle";
      #       shift-l = "vim::WindowBottom";
      #       q = "vim::ToggleRecord";
      #       shift-q = "vim::ReplayLastRecording";
      #       "@" = "vim::PushReplayRegister";
      #       "z enter" = [ "workspace::SendKeystrokes" "z t ^" ];
      #       "z -" = [ "workspace::SendKeystrokes" "z b ^" ];
      #       "z ^" = [ "workspace::SendKeystrokes" "shift-h k z b ^" ];
      #       "z +" = [ "workspace::SendKeystrokes" "shift-l j z t ^" ];
      #       "z t" = "editor::ScrollCursorTop";
      #       "z z" = "editor::ScrollCursorCenter";
      #       "z ." = [ "workspace::SendKeystrokes" "z z ^" ];
      #       "z b" = "editor::ScrollCursorBottom";
      #       "z a" = "editor::ToggleFold";
      #       "z shift-a" = "editor::ToggleFoldRecursive";
      #       "z c" = "editor::Fold";
      #       "z shift-c" = "editor::FoldRecursive";
      #       "z o" = "editor::UnfoldLines";
      #       "z shift-o" = "editor::UnfoldRecursive";
      #       "z f" = "editor::FoldSelectedRanges";
      #       "z shift-m" = "editor::FoldAll";
      #       "z shift-r" = "editor::UnfoldAll";
      #       "shift-z shift-q" = [ "pane::CloseActiveItem" { save_intent = "skip"; } ];
      #       "shift-z shift-z" = [ "pane::CloseActiveItem" { save_intent = "save_all"; } ];
      #       "ctrl-w d" = "editor::GoToDefinitionSplit";
      #       "ctrl-w g d" = "editor::GoToDefinitionSplit";
      #       "ctrl-w shift-d" = "editor::GoToTypeDefinitionSplit";
      #       "ctrl-w g shift-d" = "editor::GoToTypeDefinitionSplit";
      #       "ctrl-w space" = "editor::OpenExcerptsSplit";
      #       "ctrl-w g space" = "editor::OpenExcerptsSplit";
      #       ctrl-6 = "pane::AlternateFile";
      #       escape = "editor::Cancel";
      #       "ctrl-[" = "editor::Cancel";
      #       ":" = "command_palette::Toggle";
      #       "." = "vim::Repeat";
      #       shift-d = "vim::DeleteToEndOfLine";
      #       shift-j = "vim::JoinLines";
      #       y = "editor::Copy";
      #       shift-y = "vim::YankLine";
      #       i = "vim::InsertBefore";
      #       shift-i = "vim::InsertFirstNonWhitespace";
      #       a = "vim::InsertAfter";
      #       shift-a = "vim::InsertEndOfLine";
      #       x = "vim::DeleteRight";
      #       shift-x = "vim::DeleteLeft";
      #       o = "vim::InsertLineBelow";
      #       shift-o = "vim::InsertLineAbove";
      #       "~" = "vim::ChangeCase";
      #       ctrl-a = "vim::Increment";
      #       ctrl-x = "vim::Decrement";
      #       p = "vim::Paste";
      #       shift-p = [ "vim::Paste" { before = true; } ];
      #       u = "vim::Undo";
      #       ctrl-r = "vim::Redo";
      #       r = "vim::PushReplace";
      #       s = "vim::Substitute";
      #       shift-s = "vim::SubstituteLine";
      #       ">" = "vim::PushIndent";
      #       "<" = "vim::PushOutdent";
      #       "=" = "vim::PushAutoIndent";
      #       "g u" = "vim::PushLowercase";
      #       "g shift-u" = "vim::PushUppercase";
      #       "g ~" = "vim::PushOppositeCase";
      #       "\"" = "vim::PushRegister";
      #       "g q" = "vim::PushRewrap";
      #       "g w" = "vim::PushRewrap";
      #       ctrl-pagedown = "pane::ActivateNextItem";
      #       ctrl-pageup = "pane::ActivatePreviousItem";
      #       insert = "vim::InsertBefore";
      #       "[ x" = "editor::SelectLargerSyntaxNode";
      #       "] x" = "editor::SelectSmallerSyntaxNode";
      #       "] d" = "editor::GoToDiagnostic";
      #       "[ d" = "editor::GoToPreviousDiagnostic";
      #       "] c" = "editor::GoToHunk";
      #       "[ c" = "editor::GoToPreviousHunk";
      #       "g n" = "pane::ActivateNextItem";
      #       "g p" = "pane::ActivatePreviousItem";
      #       H = "pane::ActivatePreviousItem";
      #       L = "pane::ActivateNextItem";
      #       "g l" = "vim::EndOfLine";
      #       "g h" = "vim::StartOfLine";
      #       "g s" = "vim::FirstNonWhitespace";
      #       "g e" = "vim::EndOfDocument";
      #       "g y" = "editor::GoToTypeDefinition";
      #       "g r" = "editor::FindAllReferences";
      #       "g t" = "vim::WindowTop";
      #       "g c" = "vim::WindowMiddle";
      #       "g b" = "vim::WindowBottom";
      #       "space w h" = "workspace::ActivatePaneLeft";
      #       "space w l" = "workspace::ActivatePaneRight";
      #       "space w k" = "workspace::ActivatePaneUp";
      #       "space w j" = "workspace::ActivatePaneDown";
      #       "space w q" = "pane::CloseActiveItem";
      #       "space w s" = "pane::SplitRight";
      #       "space w r" = "pane::SplitRight";
      #       "space w v" = "pane::SplitDown";
      #       "space w d" = "pane::SplitDown";
      #       "space f" = "file_finder::Toggle";
      #       "space k" = "editor::Hover";
      #       "space s" = "outline::Toggle";
      #       "space shift-s" = "project_symbols::Toggle";
      #       "space d" = "editor::GoToDiagnostic";
      #       "space shift-d" = "diagnostics::Deploy";
      #       "space r" = "editor::Rename";
      #       "space a" = "editor::ToggleCodeActions";
      #       "space h" = "editor::SelectAllMatches";
      #       "space c" = "editor::ToggleComments";
      #       "space y" = "editor::Copy";
      #       "space p" = "editor::Paste";
      #       "m m" = "vim::Matching";
      #       "m i w" = [ "workspace::SendKeystrokes" "v i w" ];
      #       ctrl-k = "editor::MoveLineUp";
      #       ctrl-j = "editor::MoveLineDown";
      #       shift-u = "editor::Redo";
      #       ctrl-c = "editor::ToggleComments";
      #       d = [ "workspace::SendKeystrokes" "y d" ];
      #       c = [ "workspace::SendKeystrokes" "d i" ];
      #     };
      #   }
      #   {
      #     context = "Editor && vim_mode == visual || vim_mode == helix_normal && !VimWaiting && !VimObject";
      #     bindings = {
      #       x = "editor::SelectLine";
      #     };
      #   }
      # ];
    };
    xdg.configFile."zed/tasks.json".text = (builtins.toJSON cfg.tasks);
  };
}
