{ inputs, ... }:

{
  flake-file.inputs.home-manager-master.url = "github:nix-community/home-manager";
  flake-file.inputs.home-manager-master.inputs.nixpkgs.follows = "nixpkgs";

  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.pi = { pkgs, inputs, ... }: {
    imports = [
      "${inputs.home-manager-master}/modules/programs/pi-coding-agent.nix"
    ];

    programs.pi-coding-agent = {
      enable = true;
      package = pkgs.llm-agents.pi;
      extraPackages = with pkgs; [
        gnugrep
        findutils
        git
        fd
        ripgrep
        nodejs
        python3
        jujutsu
        nix
        direnv
        go
        golangci-lint
        gcc
        curl
        gnused
        gnumake
        pkgs.llm-agents.codegraph
        pkgs.llm-agents.semble
      ];
      settings = {
        lastChangelogVersion = "0.81.1";
        defaultProvider = "llamaswap";
        defaultModel = "qwen27";
        defaultThinkingLevel = "medium";
        hideThinkingBlock = false;
        quietStartup = false;
        treeFilterMode = "all";
        enableInstallTelemetry = false;
        packages = [
          "npm:pi-powerline-footer"
          "npm:pi-mcp-adapter"
          "npm:@juicesharp/rpiv-todo"
          "npm:@juicesharp/rpiv-ask-user-question"
          "git:github.com/jonjonrankin/pi-caveman"
          "npm:pi-direnv"
          "npm:@juicesharp/rpiv-i18n"
          "npm:@juicesharp/rpiv-web-tools"
          "npm:@juicesharp/rpiv-args"
          "npm:pi-effort"
        ];
        compaction = {
          enabled = true;
          reserveTokens = 8192;
          keepRecentTokens = 28000;
        };
        powerline = "default";
        theme = "dark";
      };
      models = {
        providers = {
          llamaswap = {
            baseUrl = "https://llama.fedeizzo.dev/v1";
            api = "openai-completions";
            apiKey = "placeholder";
            compat = {
              supportsDeveloperRole = false;
              supportsReasoningEffort = false;
            };
            models = [
              { id = "qwen36-35b-a3b"; }
              {
                id = "qwen36-27b-realtime";
                reasoning = true;
                input = [ "text" "image" ];
                contextWindow = 100000;
                maxTokens = 8192;
                cost = { input = 0; output = 0; cacheRead = 0; cacheWrite = 0; };
              }
              {
                id = "qwen27";
                reasoning = true;
                contextWindow = 100000;
                thinkingLevelMap = {
                  off = "off";
                  minimal = null;
                  low = "low";
                  medium = "medium";
                  high = "xhigh";
                  xhigh = null;
                  max = null;
                };
                compat = {
                  thinkingFormat = "chat-template";
                  chatTemplateKwargs = {
                    enable_thinking = { "$var" = "thinking.enabled"; };
                    reasoning_effort = { "$var" = "thinking.effort"; omitWhenOff = true; };
                  };
                };
              }
              {
                id = "deepseek/deepseek-v4-flash-0731";
                reasoning = true;
                contextWindow = 100000;
                thinkingLevelMap = {
                  off = "off";
                  minimal = null;
                  low = "low";
                  medium = "medium";
                  high = "xhigh";
                  xhigh = null;
                  max = null;
                };
                compat = {
                  thinkingFormat = "chat-template";
                  chatTemplateKwargs = {
                    enable_thinking = { "$var" = "thinking.enabled"; };
                    reasoning_effort = { "$var" = "thinking.effort"; omitWhenOff = true; };
                  };
                };
              }
              {
                id = "laguna";
                reasoning = true;
                input = [ "text" ];
                contextWindow = 100000;
                maxTokens = 8192;
                cost = { input = 0; output = 0; cacheRead = 0; cacheWrite = 0; };
              }
              {
                id = "ds4";
                reasoning = true;
                input = [ "text" ];
                contextWindow = 200000;
                maxTokens = 8192;
                cost = { input = 0; output = 0; cacheRead = 0; cacheWrite = 0; };
              }
            ];
          };
        };
      };
    };

    home.file = {
      ".pi/agent/caveman.json".text = builtins.toJSON {
        defaultLevel = "ultra";
        showStatus = true;
      };
      ".pi/agent/skills".source = ./config/agent/skills;
      ".pi/agent/extensions".source = ./config/agent/extensions;
    };
  };

}
