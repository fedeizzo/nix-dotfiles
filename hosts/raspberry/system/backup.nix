{ config, ... }:

{
  services.restic.backups = {
    backblaze = {
      user = "root";
      initialize = true;
      # Env file structure
      # B2_ACCOUNT_ID=
      # B2_ACCOUNT_KEY=
      environmentFile = "/root/.restic_backup_env";
      repositoryFile = config.sops.secrets.restic-repository.path;
      passwordFile = config.sops.secrets.restic-password.path;
      paths = [
        "/var/volumes"
        "/var/container_envs"
        # "/var/lib/sops"
        # "/borgbackups"
      ];
      pruneOpts = [
        "--keep-daily 1"
        "--keep-weekly 2"
        "--keep-monthly 2"
        "--keep-yearly 2"
      ];
    };
  };
}
