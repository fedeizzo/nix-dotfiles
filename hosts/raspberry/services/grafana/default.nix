_:

{
  services.grafana = {
    enable = true;
    dataDir = "/var/volumes/grafana";
    settings = {
      security = {
        admin_user = "fedeizzo";
        admin_email = "grafana@fedeizzo.dev";
        cookie_secure = true;
        disable_gravatar = true;
      };
      users = {
        allow_sign_up = false;
        allow_org_create = false;
      };
      server = {
        http_port = 50002;
      };
    };
  };
}

# list of values set manually
# security.secret_key
# security.admin_password
