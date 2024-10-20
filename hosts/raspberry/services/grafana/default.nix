_:

{
  services.grafana = {
    enable = true;
    dataDir = "/var/volumes/grafana";
    settings = {
      security = {
        admin_user = "fedeizzo";
        admin_email = "grafana@fedeizzo.dev";
        disable_gravatar = true;
        cookie_secure = true; # cookie behind https
        cookie_samesite = "strict"; # cookie only from same site: https://grafana.com/docs/grafana/latest/setup-grafana/configure-security/configure-security-hardening/#add-a-samesite-attribute-to-cookies
        login_cookie_name = "__Host-grafana_session"; # https://grafana.com/docs/grafana/latest/setup-grafana/configure-security/configure-security-hardening/#add-a-prefix-to-cookie-names
        hide_version = true; # grafana version is not provided to unauthenticated users
        enforce_domain = true; # prevent DNS rebind attacks;
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
