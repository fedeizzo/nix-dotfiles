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
      package = inputs.llm-agents.packages.${pkgs.system}.pi;
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
        inputs.llm-agents.packages.${pkgs.system}.codegraph
        inputs.llm-agents.packages.${pkgs.system}.semble
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
          # --- Context & Token Optimization ---
          # Dynamic compaction, sandboxing, and history pruning to keep KV cache lean
          "npm:context-mode"
          "npm:pi-context"

          # --- Interaction & Workflow Extensions ---
          # Structured agent interactions, side chats, argument expansion, and web access
          "npm:@juicesharp/rpiv-ask-user-question"
          "npm:@juicesharp/rpiv-btw"
          "npm:@juicesharp/rpiv-todo"
          "npm:@juicesharp/rpiv-web-tools"
          "npm:@juicesharp/rpiv-args"
          "npm:@juicesharp/rpiv-i18n"

          # --- Model Control & Prompt Steering ---
          # Thinking effort toggling and behavioral/style constraints
          "npm:pi-effort"
          "git:github.com/jonjonrankin/pi-caveman"

          # --- Synchronous Subagent Orchestration ---
          # Blocking subagents with fork-context inheritance (parent context + cache-warm)
          # Config: ~/.pi/agent/extensions/subagent/config.json (asyncByDefault:false, defaultSubagentContext:fork)
          # Model/thinking routing: subagents.* keys below in settings
          "npm:pi-subagents"

          # --- Safety & Guardrails ---
          # Filesystem boundary enforcement

          # --- Environment & Shell Integration ---
          # Automatic per-directory environment variables
          "npm:pi-direnv"

          # --- UI & Statusline ---
          # Terminal enhancements and footer statistics
          "npm:pi-powerline-footer"
        ];
        compaction = {
          enabled = true;
          reserveTokens = 8192;
          keepRecentTokens = 28000;
        };
        # Subagent model/thinking routing (pi-subagents reads these from Pi settings).
        # Precedence: per-run override > agent frontmatter > agentOverrides > defaultModel > parent model.
        # Orchestrator stays on qwen27 (65k ctx, fast); subagents get bigger context for deep work.
        # Lean-orchestrator contract: orchestrator passes output:"<agent>.md" + outputMode:"file-only"
        # on every subagent call, so reports land under ~/.pi/subagent-outputs/ and only a compact
        # file reference returns to the orchestrator. Agents below with a frontmatter `output` path
        # are forced file-only as a safety net; the others rely on the per-call output arg.
        subagents = {
          defaultModel = "deepseek/deepseek-v4-flash-vision-exp";
          defaultThinking = "high";
          agentOverrides = {
            oracle = {
              model = "deepseek/deepseek-v4-flash-vision-exp";
              # model ="z-ai/glm-5.2";
              thinking = "xhigh";
            };
            # Report agents ship a frontmatter `output` path; force file-only so the
            # orchestrator receives only a compact reference instead of full output.
            scout = {
              outputMode = "file-only";
            };
            researcher = {
              outputMode = "file-only";
            };
          };
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
              {
                id = "qwen3.8-27b";
                reasoning = true;
                contextWindow = 131072;
                maxTokens = 16384; # Crucial: gives the model enough runway to finish thinking

                thinkingLevelMap = {
                  off = null; # omit / set null so omitWhenOff takes effect cleanly
                  minimal = "low";
                  low = "low";
                  medium = "medium";
                  high = "xhigh";
                  xhigh = "xhigh";
                  max = "xhigh";
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
                id = "ds4";
                reasoning = true;
                contextWindow = 131072;
                maxTokens = 16384; # Crucial: gives the model enough runway to finish thinking

                thinkingLevelMap = {
                  off = null; # omit / set null so omitWhenOff takes effect cleanly
                  minimal = "low";
                  low = "low";
                  medium = "medium";
                  high = "high";
                  xhigh = "high";
                  max = "high";
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
                id = "deepseek/deepseek-v4-flash-vision-exp";
                reasoning = true;
                contextWindow = 200000;
                maxTokens = 16384;

                thinkingLevelMap = {
                  off = null;
                  minimal = "low";
                  low = "low";
                  medium = "high";
                  high = "high";
                  xhigh = "max";
                  max = "max";
                };

                compat = {
                  thinkingFormat = "openrouter"; # OpenRouter-native reasoning:{effort} — matches glm-5.2 on same peer
                  supportsReasoningEffort = true; # Restore effort mapping (medium->high, high->high, xhigh->max)
                };
              }
              {
                id = "z-ai/glm-5.2";
                reasoning = true;
                contextWindow = 200000;
                maxTokens = 32768;

                # GLM 5.2 supports standard 'high' and 'xhigh' (which maps to max) reasoning
                thinkingLevelMap = {
                  off = null;
                  minimal = "low";
                  low = "low";
                  medium = "medium";
                  high = "high";
                  xhigh = "xhigh";
                  max = "xhigh";
                };

                compat = {
                  thinkingFormat = "openrouter";
                };
              }
            ];
          };
        };
      };
    };

    home.file = {
      ".pi/agent/skills".source = ./config/agent/skills;
      ".pi/agent/extensions".source = ./config/agent/extensions;
    };
  };

}
