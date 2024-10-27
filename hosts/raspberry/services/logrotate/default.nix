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
      # "/var/log/audit/audit.log" = {
      #   size = "10M";
      #   rotate = 5;
      #   missingok = true;
      #   notifempty = true;
      #   postrotate = ''
      #     systemctl kill -s HUP auditd
      #   '';
      # };
    };
  };
}
