{
  flake.modules.nixos.open-webui = { config, lib, pkgs, pkgs-unstable ? pkgs, ... }:
    let
      stateDir = "/persist/var/lib/open-webui";
      llamaSwap = "http://127.0.0.1:${toString config.services.llama-swap.port}/v1";
      searx = "http://127.0.0.1:${toString config.services.searx.settings.server.port}";
      domain = "chat.fedeizzo.dev";
      authentik = "https://auth.fedeizzo.dev";
    in
    {
      services.open-webui = {
        enable = true;
        package = pkgs-unstable.open-webui;
        inherit stateDir;
        port = 5300;
        host = "127.0.0.1";
        openFirewall = false;
        # only OAUTH_CLIENT_ID and OAUTH_CLIENT_SECRET, it overrides `environment`
        environmentFile = config.sops.secrets.open-webui.path;

        # most of these only seed the config DB on first start, the admin UI
        # wins afterwards (oauth.* are always read from env)
        environment = {
          # module defaults, dropped when `environment` is set
          ANONYMIZED_TELEMETRY = "False";
          DO_NOT_TRACK = "True";
          SCARF_NO_ANALYTICS = "True";
          ENABLE_VERSION_UPDATE_CHECK = "False";

          # models from llama-swap
          ENABLE_OLLAMA_API = "False";
          OPENAI_API_BASE_URLS = llamaSwap;
          OPENAI_API_KEYS = "none";
          TASK_MODEL_EXTERNAL = "lfm3-1.7b";

          # web search
          ENABLE_WEB_SEARCH = "True";
          WEB_SEARCH_ENGINE = "searxng";
          SEARXNG_QUERY_URL = "${searx}/search?q=<query>";
          WEB_SEARCH_RESULT_COUNT = "5";

          # RAG embeddings
          RAG_EMBEDDING_ENGINE = "openai";
          RAG_EMBEDDING_MODEL = "embeddinggemma-2";
          RAG_OPENAI_API_BASE_URL = llamaSwap;
          RAG_OPENAI_API_KEY = "none";

          # speech
          AUDIO_STT_ENGINE = "openai";
          AUDIO_STT_MODEL = "lfm3-audio-asr";
          AUDIO_STT_OPENAI_API_BASE_URL = llamaSwap;
          AUDIO_STT_OPENAI_API_KEY = "none";
          AUDIO_TTS_ENGINE = "openai";
          AUDIO_TTS_MODEL = "lfm3-audio-tts";
          AUDIO_TTS_VOICE = "female";
          AUDIO_TTS_OPENAI_API_BASE_URL = llamaSwap;
          AUDIO_TTS_OPENAI_API_KEY = "none";

          # image generation and editing (Turbo: no `steps`, Open WebUI's
          # openai engine does not send them)
          ENABLE_IMAGE_GENERATION = "True";
          IMAGE_GENERATION_ENGINE = "openai";
          IMAGE_GENERATION_MODEL = "qwen-image-2.1-turbo";
          IMAGE_SIZE = "1024x1024";
          IMAGES_OPENAI_API_BASE_URL = llamaSwap;
          IMAGES_OPENAI_API_KEY = "none";
          ENABLE_IMAGE_EDIT = "True";
          IMAGE_EDIT_ENGINE = "openai";
          IMAGE_EDIT_MODEL = "qwen-image-2.1-turbo";
          IMAGES_EDIT_OPENAI_API_BASE_URL = llamaSwap;
          IMAGES_EDIT_OPENAI_API_KEY = "none";

          # UI
          WEBUI_URL = "https://${domain}";
          DEFAULT_USER_ROLE = "user";
          ENABLE_COMMUNITY_SHARING = "False";
          ENABLE_EVALUATION_ARENA_MODELS = "False";

          # authentication via authentik (OIDC provider slug: open-webui)
          ENABLE_SIGNUP = "False";
          ENABLE_LOGIN_FORM = "False";
          ENABLE_PASSWORD_AUTH = "False";
          ENABLE_OAUTH_SIGNUP = "True";
          OAUTH_MERGE_ACCOUNTS_BY_EMAIL = "True";
          OAUTH_PROVIDER_NAME = "Authentik";
          OPENID_PROVIDER_URL = "${authentik}/application/o/open-webui/.well-known/openid-configuration";
          OPENID_REDIRECT_URI = "https://${domain}/oauth/oidc/callback";
          OAUTH_SCOPES = "openid email profile";
          OAUTH_USERNAME_CLAIM = "preferred_username";
          ENABLE_OAUTH_ROLE_MANAGEMENT = "True";
          OAUTH_ROLES_CLAIM = "groups";
          OAUTH_ALLOWED_ROLES = "authentik Admins,Open WebUI Users";
          OAUTH_ADMIN_ROLES = "authentik Admins";
        };
      };

      sops.secrets.open-webui = {
        format = "dotenv";
        mode = "0400";
        restartUnits = [
          "open-webui.service"
        ];
        sopsFile = ./open-webui-homelab-secrets.env;
        key = ""; # to map the whole file as a secret
      };
      # StateDirectory is disabled below, so create the state dir ourselves
      systemd.tmpfiles.rules = [
        "d ${stateDir} 0700 open-webui open-webui -"
        "d ${stateDir}/data 0700 open-webui open-webui -"
      ];

      # for consistent backup
      systemd.services.open-webui.serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "open-webui";
        Group = "open-webui";

        StateDirectory = lib.mkForce "";
        WorkingDirectory = "/persist/var/lib/open-webui";
      };

      fi.services = [
        {
          name = "open-webui";
          subdomain = "chat";
          port = config.services.open-webui.port;
          dashboardSection = "Tools";
          toPersist = [
            # {
            #   directory = "${config.services.open-webui.stateDir}";
            #   user = "open-webui";
            #   group = "open-webui";
            #   mode = "u=rwx,g=,o=";
            # }
          ];
          toBackup = [
            "/persist/var/lib/open-webui/data"
          ];
        }
      ];

      users.users.open-webui = {
        uid = 311;
        group = "open-webui";
      };
      users.groups.open-webui.gid = 312;
    };
}
