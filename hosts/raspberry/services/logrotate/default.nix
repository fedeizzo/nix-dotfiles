_:

{
  services.logrotate = {
    enable = true;
    settings = {
      "/var/volumes/traefik/log/access.json" = {
        size = "100M";
        rotate = 5;
        missingok = true;
        notifempty = true;
        postrotate = ''
          systemctl kill --signal=USR1 traefik
        '';
      };
    };
  };
}
