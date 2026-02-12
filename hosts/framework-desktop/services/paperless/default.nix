{ config, lib, pkgs, ... }:

{
  users.users.paperless.uid = 315; # make backup consistent across machines
  users.groups.paperless.gid = 315; # make backup consistent across machines
  users.users.paperless.group = "paperless";
  services.paperless = {
    enable = true;
    address = "0.0.0.0";
    port = 28981;
    dataDir = "/var/lib/paperless";
    user = "paperless";
    consumptionDirIsPublic = false;
    package = pkgs.paperless-ngx;

    database.createLocally = false; # self managed
    environmentFile = "${config.sops.secrets.paperless.path}";
    passwordFile = "${config.sops.secrets.paperless-admin-password.path}";
    settings = {
      PAPERLESS_DBENGINE = "postgresql";
      PAPERLESS_DBHOST = "localhost";
      PAPERLESS_DBPORT = "5432";
      PAPERLESS_DBUSER = "paperless"; # password in the env file

      PAPERLESS_ACCOUNT_ALLOW_SIGNUPS = "False";
      PAPERLESS_SOCIALACCOUNT_ALLOW_SIGNUPS = "False";

      PAPERLESS_OCR_LANGUAGE = "eng+fra+ita";
      PAPERLESS_OCR_USER_ARGS = {
        optimize = 1;
        pdfa_image_compression = "lossless";
        invalidate_digital_signatures = true;
      };

      PAPERLESS_FILENAME_FORMAT = "{{ correspondent }}/{{ document_type }}/{{ created_year }}/{{ title }}";

      PAPERLESS_TASK_WORKERS = 4;
      PAPERLESS_THREADS_PER_WORKER = 2;

      PAPERLESS_TRAIN_TASK_CRON = "40 */1 * * *";

      PAPERLESS_CONSUMER_DISABLE = "False";

      PAPERLESS_ENABLE_NLTK = false;

      # oauth
      PAPERLESS_ENABLE_ALLAUTH = true;
      PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect";
      PAPERLESS_DISABLE_REGULAR_LOGIN = true;
    };
  };
  virtualisation.oci-containers.containers."paperless-gpt" = {
    image = "icereed/paperless-gpt:latest";
    autoStart = true;
    extraOptions = [ "--network=host" ];
    ports = [ ];
    environmentFiles = [ "${config.sops.secrets.paperless-gpt.path}" ];
    environment = {
      PAPERLESS_BASE_URL = "https://paperless.fedeizzo.dev";
      LLM_PROVIDER = "openai";
      LLM_MODEL = "qwen/qwen3-8b";
      OPENAI_API_KEY = "placeholder";
      OPENAI_BASE_URL = "https://llm.fedeizzo.dev/v1";
      LISTEN_INTERFACE = ":28982";
      # OCR_PROVIDER="llm";
      # VISION_LMM_PROVIDER="llm";
      # VISION_LLM_MODEL=todo
    };
  };
  virtualisation.oci-containers.containers."paperless-ai" = {
    image = "clusterzx/paperless-ai:latest";
    autoStart = true;
    extraOptions = [ "--network=host" ];
    ports = [ ];
    volumes = [
      "/var/lib/paperless-ai:/app/data"
    ];
    environmentFiles = [ "${config.sops.secrets.paperless-gpt.path}" ];
    environment = {
      PAPERLESS_AI_INITIAL_SETUP = "yes";
      PAPERLESS_AI_PORT = "28983";
      PAPERLESS_API_URL = "https://paperless.fedeizzo.dev/api";
      PAPERLESS_USERNAME = "trackpadblue";
      AI_PROVIDER = "custom";
      CUSTOM_API_KEY = "placeholder";
      CUSTOM_BASE_URL = "https://llm.fedeizzo.dev/v1";
      # CUSTOM_MODEL = "mistralai/devstral-small-2-2512";
      # CUSTOM_MODEL = "qwen/qwen3-8b";
      SCAN_INTERVAL = "*/30 * * * *";
    };
  };
  sops.secrets.paperless = lib.mkIf config.services.paperless.enable {
    format = "dotenv";
    mode = "0400";
    owner = config.users.users.paperless.name;
    group = config.users.groups.paperless.name;
    restartUnits = [
      "paperless-consumer.service"
      "paperless-scheduler.service"
      "paperless-task-queue.service"
      "paperless-web.service"
    ];
    sopsFile = ./paperless-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
  sops.secrets.paperless-gpt = lib.mkIf config.services.paperless.enable {
    format = "dotenv";
    mode = "0400";
    restartUnits = [ "docker-paperless-gpt.service" ];
    sopsFile = ./paperless-gpt-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
