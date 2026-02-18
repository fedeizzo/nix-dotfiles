{ config, ... }:

{
  services.grafana = {
    enable = true;
    dataDir = "/var/volumes/grafana";
    settings = {
      server = {
        root_url = "https://grafana.fedeizzo.dev";
      };
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
      auth = {
        signout_redirect_url = "https://auth.fedeizzo.dev/application/o/grafana/end-session/";
        oauth_auto_login = true;
        oauth_allow_insecure_email_lookup = true;
      };
      "auth.basic" = {
        enabled = false;
      };
      "auth.generic_oauth" = {
        name = "authentik";
        enabled = true;
        allow_sign_up = false;
        auto_login = true;
        allow_assign_grafana_admin = true;
        role_attribute_path = "contains(groups, 'authentik Admins') && 'GrafanaAdmin' || contains(groups, 'Grafana Editors') && 'Editor' || 'Viewer'";
        client_id = "$__file{${config.sops.secrets.grafana-oauth-client-id.path}}";
        client_secret = "$__file{${config.sops.secrets.grafana-oauth-client-secret.path}}";
        scopes = "openid email profile";
        auth_url = "https://auth.fedeizzo.dev/application/o/authorize/";
        token_url = "https://auth.fedeizzo.dev/application/o/token/";
        api_url = "https://auth.fedeizzo.dev/application/o/userinfo/";
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
  sops.secrets.grafana-oauth-client-id = {
    format = "yaml";
    mode = "0440";
    owner = "grafana";
    group = "grafana";
    sopsFile = ./grafana-homelab-secrets.yaml;
  };
  sops.secrets.grafana-oauth-client-secret = {
    format = "yaml";
    mode = "0440";
    owner = "grafana";
    group = "grafana";
    sopsFile = ./grafana-homelab-secrets.yaml;
  };
}

# list of values set manually
# security.secret_key
# security.admin_password
