{ config, pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    checkConfig = true; # check config at compile time
    initdbArgs = [
      "--data-checksums"
    ];

    ensureDatabases = [ "sftpgo" "test" ];
    ensureUsers = [
      { name = "sftpgo"; ensureDBOwnership = true; }
      { name = "test"; }
    ];
    authentication = pkgs.lib.mkOverride 10 ''
      #type  database  DBuser    auth-method  optional_ident_map
      local  test      test      md5
      local  all       postgres  trust

      # local db      user     peer        map=superuser_map
    '';
    identMap = ''
      superuser_map      sftpgo      sftpgo
    '';
  };

  systemd.services.postgresql.postStart =
    ''
      $PSQL -tA <<'EOF'
        DO $$
        DECLARE sftpgo_password TEXT;
        DECLARE test_password TEXT;
        BEGIN
          sftpgo_password := trim(both from replace(pg_read_file('${config.sops.secrets.sftpgo-pg-password.path}'), E'\n', '''));
          test_password := trim(both from replace(pg_read_file('${config.sops.secrets.sftpgo-pg-password.path}'), E'\n', '''));
          EXECUTE 'ALTER ROLE sftpgo WITH PASSWORD '''sftpgo-password''';';
          EXECUTE format('ALTER ROLE test WITH PASSWORD '''%s''';', sftpgo_password);
        END $$;
      EOF
    '';
}
