{ lib, ... }:

{
  services.ntfy-sh = {
    enable = true;
    settings = {
      # Server
      base-url = "https://ntfy.fedeizzo.dev";
      listen-http = "127.0.0.1:23445";
      behind-proxy = true;

      # Access control
      auth-file = "/var/lib/ntfy-sh/auth.db";
      enable-login = true;
      enable-signup = true;

      # Attachments
      attachment-cache-dir = "/var/cache/ntfy-sh/attachments";

      # Message cache
      cache-file = "/var/cache/ntfy-sh/cache.db";
    };
  };
  systemd.services.ntfy-sh.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "ntfy-sh";
    Group = "ntfy-sh";
  };
}
