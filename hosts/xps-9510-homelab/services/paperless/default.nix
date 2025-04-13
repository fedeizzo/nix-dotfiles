{ config, lib, ... }:

{
  services.paperless = {
    enable = true;
    address = "0.0.0.0";
    port = 28981;
    dataDir = "/var/lib/paperless";
    user = "paperless";
    consumptionDirIsPublic = false;

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
}
