{ inputs, ... }:
let
  defaultInstructions = builtins.readFile ./worker-instructions.md;
  mkEnvironment = { pkgs, modelSocket, instructionsText ? defaultInstructions }:
    let
      lib = pkgs.lib;
      tools = with pkgs; [ bash coreutils git jujutsu ripgrep findutils gnugrep gnused diffutils cargo rustc gcc gnumake nix ];
      toolPath = lib.makeBinPath tools;
      json = pkgs.formats.json { };
      settings = json.generate "pan-pi-settings.json" {
        packages = [ ];
        enableInstallTelemetry = false;
        quietStartup = true;
        retry.enabled = false;
      };
      models = json.generate "pan-pi-models.json" {
        providers.pan-local = {
          baseUrl = "http://127.0.0.1:11435/v1";
          api = "openai-completions";
          apiKey = "placeholder";
          compat = {
            supportsDeveloperRole = false;
            supportsReasoningEffort = false;
          };
          models = [{
            id = "qwen-flash";
            reasoning = true;
            contextWindow = 65536;
            maxTokens = 16384;
          }];
        };
      };
      instructions = pkgs.writeText "pan-pi-instructions.md" instructionsText;
      # Fetching has network access, but receives no prompts or agent extensions.
      fetch = pkgs.writeShellScript "pan-dev-fetch" ''
        workspace=$1
        shift
        exec ${pkgs.bubblewrap}/bin/bwrap \
          --unshare-all --share-net --die-with-parent --new-session --cap-drop ALL \
          --ro-bind /nix/store /nix/store --proc /proc --dev /dev \
          --tmpfs /tmp --dir /etc --dir /bin \
          --symlink ${pkgs.bash}/bin/bash /bin/sh \
          --ro-bind /etc/resolv.conf /etc/resolv.conf \
          --ro-bind ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt \
          --bind "$workspace" /workspace --chdir /workspace \
          --setenv HOME /tmp --setenv GIT_CONFIG_NOSYSTEM 1 \
          --setenv GIT_CONFIG_GLOBAL /dev/null --setenv GIT_TERMINAL_PROMPT 0 \
          --setenv SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt \
          --setenv GIT_SSL_CAINFO /etc/ssl/certs/ca-certificates.crt \
          "$@"
      '';
      sandbox = pkgs.writeShellScript "pan-dev-sandbox" ''
        workspace=$1
        shift
        exec ${pkgs.bubblewrap}/bin/bwrap \
          --unshare-all --die-with-parent --new-session --cap-drop ALL \
          --ro-bind /nix/store /nix/store --proc /proc --dev /dev \
          --tmpfs /tmp --tmpfs /agent --dir /config --dir /bin \
          --symlink ${pkgs.bash}/bin/bash /bin/sh \
          --symlink ${pkgs.bash}/bin/bash /bin/bash \
          --ro-bind ${settings} /agent/settings.json \
          --ro-bind ${models} /agent/models.json \
          --ro-bind ${instructions} /config/instructions.md \
          --ro-bind "${modelSocket}" /model.sock \
          --bind "$workspace" /workspace --chdir /workspace \
          --setenv HOME /tmp --setenv PI_CODING_AGENT_DIR /agent \
          --setenv PI_OFFLINE 1 --setenv PI_TELEMETRY 0 \
          --setenv GIT_CONFIG_NOSYSTEM 1 --setenv GIT_CONFIG_GLOBAL /dev/null \
          --setenv JJ_CONFIG /config/jj.toml \
          --ro-bind ${pkgs.writeText "pan-worker-jj.toml" ''
            [user]
            name = "Pan development worker"
            email = "pan-dev@localhost"
            [signing]
            behavior = "drop"
          ''} /config/jj.toml \
          ${pkgs.bash}/bin/bash -c '
            ${pkgs.socat}/bin/socat TCP4-LISTEN:11435,bind=127.0.0.1,reuseaddr,fork UNIX-CONNECT:/model.sock &
            exec "$@"
          ' pan-sandbox "$@"
      '';
    in
    { inherit fetch sandbox toolPath; };
in
{
  flake.modules.nixos.apps-pan-worker = { pkgs, config, lib, ... }:
    let
      cfg = config.services.apps-pan;
      worker = cfg.development;
      pi = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
      runtime = "/run/pan-dev";
      state = "/var/lib/pan-dev";
      modelSocket = "${runtime}/model.sock";
      environment = mkEnvironment { inherit pkgs modelSocket; instructionsText = worker.instructions; };
      inherit (environment) fetch sandbox toolPath;
      yaml = pkgs.formats.yaml { };
      workerConfig = yaml.generate "pan-worker.yaml" {
        socket = "${runtime}/control.sock";
        state_dir = state;
        sandbox_program = sandbox;
        fetch_program = fetch;
        pi_program = "${pi}/bin/pi";
        git_program = "${pkgs.git}/bin/git";
        jj_program = "${pkgs.jujutsu}/bin/jj";
        path = toolPath;
        timeout_seconds = worker.timeoutSeconds;
        repositories = worker.repositories;
      };
    in
    {
      options.services.apps-pan.development = {
        enable = lib.mkEnableOption "Pan's isolated Pi development worker";
        repositories = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule {
            options = {
              url = lib.mkOption { type = lib.types.str; };
              branch = lib.mkOption { type = lib.types.str; default = "master"; };
            };
          });
          default.pan = { url = "https://github.com/fedeizzo/nix-dotfiles.git"; branch = "master"; };
          description = "Allowlisted public HTTPS repositories. Jobs receive no GitHub credentials.";
        };
        timeoutSeconds = lib.mkOption {
          type = lib.types.ints.positive;
          default = 1800;
          description = "Maximum total time for one development job.";
        };
        instructions = lib.mkOption {
          type = lib.types.lines;
          default = defaultInstructions;
          description = "Dedicated worker guidance, independent of the user's personal Pi configuration.";
        };
      };

      config = lib.mkIf (cfg.enable && worker.enable) {
        assertions = [{
          assertion = config.services.llama-swap.enable;
          message = "Pan's development worker requires local llama-swap.";
        }];
        services.apps-pan.settings.development.socket = "${runtime}/control.sock";
        users.groups.pan-dev = { };
        users.groups.pan-dev-client = { };
        users.users.pan-dev = { isSystemUser = true; group = "pan-dev"; };
        users.users.pan-rust.extraGroups = [ "pan-dev-client" ];
        systemd.tmpfiles.rules = [ "d ${runtime} 0750 pan-dev pan-dev-client -" ];

        systemd.sockets.pan-dev-model = {
          wantedBy = [ "sockets.target" ];
          socketConfig = {
            ListenStream = modelSocket;
            SocketUser = "pan-dev";
            SocketGroup = "pan-dev";
            SocketMode = "0600";
            RemoveOnStop = true;
          };
        };
        systemd.services.pan-dev-model = {
          description = "Pan development model bridge";
          serviceConfig = {
            ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${toString config.services.llama-swap.port}";
            User = "pan-dev";
            Group = "pan-dev";
            NoNewPrivileges = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            PrivateTmp = true;
          };
        };
        systemd.services.pan-dev = {
          description = "Pan development worker";
          wantedBy = [ "multi-user.target" ];
          requires = [ "pan-dev-model.socket" ];
          after = [ "network-online.target" "pan-dev-model.socket" "systemd-tmpfiles-setup.service" ];
          wants = [ "network-online.target" ];
          serviceConfig = {
            ExecStart = "${cfg.package}/bin/pan worker --worker-config ${workerConfig}";
            User = "pan-dev";
            Group = "pan-dev-client";
            StateDirectory = "pan-dev";
            StateDirectoryMode = "0700";
            WorkingDirectory = state;
            Restart = "on-failure";
            KillMode = "control-group";
            TimeoutStopSec = 10;
            MemoryMax = "8G";
            TasksMax = 256;
            CPUQuota = "200%";
            UMask = "0077";
            NoNewPrivileges = true;
            ProtectSystem = "strict";
            ProtectHome = true;
            PrivateTmp = true;
            PrivateDevices = true;
            ProtectKernelTunables = true;
            ProtectKernelModules = true;
            ProtectControlGroups = true;
            RestrictSUIDSGID = true;
            CapabilityBoundingSet = "";
            InaccessiblePaths = [ "-/run/secrets" "-/run/user" "-/run/dbus" "-/run/systemd/private" "-/run/current-system" "-/var/lib/pan-rust" "-/var/lib/pan" ];
          };
        };
        fi.services = [{
          name = "pan-dev";
          shouldBehindReverseProxy = false;
          shouldMonitorUptime = false;
          shouldBeInDashboard = false;
          toPersist = [{ directory = state; user = "pan-dev"; group = "pan-dev-client"; mode = "0700"; }];
        }];
      };
    };
}
