_:

{
  services.sftpgo = {
    enable = true;
    dataDir = "/var/volumes/sftpgo";

    settings = {
      common = {
        upload_mode = 0; # upload directly done in the filesystem without using a temporary file
        setstat_mode = 0; # it's possible to change file permissions
        max_total_connections = 5;
        max_per_host_connections = 20;

        defender = {
          # brute force prevention
          enabled = true;
          driveer = "memory";
        };
      };

      data_provider = {
        driver = "sqlite";
        name = "/var/volumes/sftpgo/sftpgo.db";

        # The following fields are empty because I'm using sqlite
        host = "";
        port = "";
        username = "";
        password = "";
      };

      mfa = {
        totp = {
          algo = "sha512";
        };
      };
      sftpd = {
        bindings = [
          {
            port = 2022;
            address = "0.0.0.0";
          }
        ];
      };
      httpd = {
        bindings = [
          {
            port = 50006;
            address = "0.0.0.0";
            enable_web_admin = false;
            enable_web_client = true;
            enable_rest_api = false;
          }
        ];
      };
      telemetry = {
        bind_port = 50007;
        enable_profiler = true;
      };
    };
  };
}
