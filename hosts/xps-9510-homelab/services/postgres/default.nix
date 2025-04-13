{ config, pkgs, ... }:

{
  services.postgresqlBackup = {
    enable = true;
    location = "/var/backup/postgresql";
    backupAll = true;
    compression = "zstd";
    compressionLevel = 19;
    startAt = "*-*-* 23:00:00";
  };

  services.postgresql = {
    enable = true;
    checkConfig = true; # check config at compile time
    package = pkgs.postgresql_16;
    initdbArgs = [
      "--data-checksums"
    ];

    ensureDatabases = [
      "networth"
      "paperless"
    ];
    ensureUsers = [
      { name = "networth"; ensureDBOwnership = true; }
      { name = "networth_ro"; }
      { name = "paperless"; ensureDBOwnership = true; }
    ];
    authentication = pkgs.lib.mkForce ''
      # TYPE  DATABASE        USER            ADDRESS                 METHOD    ARGS
      host    networth        networth_ro     samehost                md5
      host    networth        networth        samehost                md5
      host    immich          immich          samehost                md5
      host    sftpgo          sftpgo          samehost                md5
      host    paperless       paperless       samehost                md5
      local   all             postgres                                trust
    '';
    # identMap = ''
    #   superuser_map      sftpgo      sftpgo
    # '';
  };

  # Everything is in post start instead of init script because post start
  # is executed every time, even when the db is already populated.
  systemd.services.postgresql.postStart =
    ''
      $PSQL -tA <<'EOF'
        DO $$
        DECLARE networth_password TEXT;
        DECLARE networth_ro_password TEXT;
        DECLARE immich_password TEXT;
        DECLARE paperless_password TEXT;
        BEGIN
          networth_password := trim(both from replace(pg_read_file('${config.sops.secrets.networth-pg-password.path}'), E'\n', '''));
          networth_ro_password := trim(both from replace(pg_read_file('${config.sops.secrets.networth-pg-password-ro.path}'), E'\n', '''));
          immich_password := trim(both from replace(pg_read_file('${config.sops.secrets.immich-pg-password.path}'), E'\n', '''));
          paperless_password := trim(both from replace(pg_read_file('${config.sops.secrets.paperless-pg-password.path}'), E'\n', '''));
          EXECUTE format('ALTER ROLE networth WITH PASSWORD '''%s''';', networth_password);
          EXECUTE format('ALTER ROLE networth_ro WITH PASSWORD '''%s''';', networth_ro_password);
          EXECUTE format('ALTER ROLE immich WITH PASSWORD '''%s''';', immich_password);
          EXECUTE format('ALTER ROLE paperless WITH PASSWORD '''%s''';', paperless_password);
        END $$;
        ${(builtins.readFile ./datbases/role_permissions.sql)}
      EOF
      $PSQL -tA -d networth <<'EOF'
        ${(builtins.readFile ./datbases/networth.sql)}
      EOF
    '';
}
