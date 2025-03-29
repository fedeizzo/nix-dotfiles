{ ... }:

{
  imports = [
    ../../../common/zed
  ];

  zed-fedeizzo = {
    enable = true;
    gopls = {
      buildFlags = [ "-tags=dynamic" ]; # makes cgo kafka work

      directoryFilters = [
        # makes gopls faster
        "-"
        "+domains/synthetics"
        "+synthetics"
      ];
    };

    tasks = [
      {
        label = "Golang test";
        command = "go test";
        cwd = "$ZED_DIRNAME";
        args = [
          "-v"
          "."
          "-tags=dynamic"
          "-run"
          "$ZED_SYMBOL"
        ];
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
        tags = [
          "go-test"
          "go-subtest"
        ];
      }
      {
        label = "Golang integration test";
        command = "go test";
        cwd = "$ZED_DIRNAME";
        args = [
          "-v"
          "."
          "-tags=dynamic"
          "-args"
          "integration"
          "-run"
          "$ZED_SYMBOL"
        ];
        use_new_terminal = false;
        allow_concurrent_runs = false;
        reveal = "always";
        reveal_target = "dock";
        hide = "on_success";
        shell = "system";
        tags = [
          "go-test"
          "go-subtest"
        ];
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
        tags = [
          "python-unittest-class"
          "python-unittest-method"
          "python-pytest-class"
          "python-pytest-method"
        ];
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
