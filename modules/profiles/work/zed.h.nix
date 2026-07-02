{
  flake.modules.homeManager.profile-work = { lib, ... }: {
    programs.zed-editor.userSettings.lsp.gopls.initialization_options = {
      directoryFilters = [ "-" "+domains/synthetics" "+synthetics" ];
    };
    programs.zed-editor.userSettings = {
      show_edit_predictions = true;
      edit_predictions = lib.mkForce {
        mode = "eager";
        provider = "ollama";
        ollama = {
          model = "qwen2.5-coder:7b";
          max_output_tokens = 128;
          api_url = "https://ai-devx-ide.us1.prod.dog/internal/ide/ollama";
        };
      };
      language_models = lib.mkForce {
        ollama = {
          api_url = "https://ai-devx-ide.us1.prod.dog/internal/ide/ollama";
        };
      };
    };

    xdg.configFile."zed/tasks.json".text = builtins.toJSON [
      {
        label = "Golang test";
        command = "go test";
        cwd = "$ZED_DIRNAME";
        args = [ "-v" "." "-tags=dynamic" "-run" "$ZED_SYMBOL" ];
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
        tags = [ "go-test" "go-subtest" ];
      }
      {
        label = "Golang integration test";
        command = "go test";
        cwd = "$ZED_DIRNAME";
        args = [ "-v" "." "-tags=dynamic" "-args" "integration" "-run" "$ZED_SYMBOL" ];
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
        tags = [ "go-test" "go-subtest" ];
      }
      {
        label = "Python test";
        command = "devc shell -- pytest -vvv --lf $ZED_RELATIVE_FILE";
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "never";
        shell = "system";
        tags = [ "python-unittest-class" "python-unittest-method" "python-pytest-class" "python-pytest-method" ];
      }
      {
        label = "Python mypy";
        command = "devc shell -- mypy --config=mypy_loose.ini $ZED_RELATIVE_FILE";
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
      }
      {
        label = "Python ruff check";
        command = "devc shell -- ruff check --fix $ZED_RELATIVE_FILE";
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
      }
      {
        label = "Python ruff format";
        command = "devc shell -- ruff format $ZED_RELATIVE_FILE";
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
      }
      {
        label = "Python isort";
        command = "devc shell -- isort $ZED_RELATIVE_FILE";
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
      }
    ];
  };
}
