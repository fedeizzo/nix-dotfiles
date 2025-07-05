{ config, pkgs, lib, ... }:

let
  dbs = [
    { user = "networth_ro"; db = "networth"; }
    { user = "networth"; db = "networth"; }
    { user = "immich"; db = "immich"; }
    { user = "paperless"; db = "paperless"; }
    { user = "nextcloud"; db = "nextcloud"; }
  ];
  authenticationEntry = user: db: "host " + db + " " + user + " samehost md5";
  passwordDeclarationEntry = user: "DECLARE " + user + "_password TEXT;";
  passwordInitEntry = user: "${user}_password := trim(both from replace(pg_read_file('${config.sops.secrets."${user}-pg-password".path}'), E\'\\n\', \'\'));";
  passwordExecuteEntry = user: "EXECUTE format('ALTER ROLE ${user} WITH PASSWORD %L;', ${user}_password);";
in
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
      "nextcloud"
    ];
    ensureUsers = [
      { name = "networth"; ensureDBOwnership = true; }
      { name = "networth_ro"; }
      { name = "paperless"; ensureDBOwnership = true; }
      { name = "nextcloud"; ensureDBOwnership = true; }
    ];
    authentication = pkgs.lib.mkForce ''
      # TYPE  DATABASE        USER            ADDRESS                 METHOD    ARGS
      ${lib.strings.concatLines (map (el: authenticationEntry el.user el.db) dbs)}
      host postgres nextcloud samehost md5
      local   all             postgres                                trust
    '';
    identMap = ''
      postgres postgres postgres
    '';
  };

  # Everything is in post start instead of init script because post start
  # is executed every time, even when the db is already populated.
  systemd.services.postgresql.postStart =
    ''
      $PSQL -tA <<'EOF'
        DO $$
        ${lib.strings.concatLines (map (el: passwordDeclarationEntry el.user) dbs)}
        BEGIN
          ${lib.strings.concatLines (map (el: passwordInitEntry el.user) dbs)}
          ${lib.strings.concatLines (map (el: passwordExecuteEntry el.user) dbs)}
        END $$;
        ${(builtins.readFile ./datbases/role_permissions.sql)}
      EOF
      $PSQL -tA -d networth <<'EOF'
        ${(builtins.readFile ./datbases/networth.sql)}
      EOF
    '';
}
