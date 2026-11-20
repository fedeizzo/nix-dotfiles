{
  # restic REST server: target for homelab's "local" restic job.
  # A plain directory repo only works on the host running restic, so homelab
  # needs a network backend to reach forge (like b2: for Backblaze).
  flake.modules.nixos.forge = { config, self, ... }:
    let
      port = 47213;
      homelabIp = (builtins.head self.nixosConfigurations.homelab.config.networking.interfaces.eth0.ipv4.addresses).address;
    in
    {
      services.restic.server = {
        enable = true;
        listenAddress = toString port;
        extraFlags = [ "--no-auth" ];
      };

      environment.persistence."/persist".directories = [
        {
          directory = config.services.restic.server.dataDir;
          user = "restic";
          group = "restic";
          mode = "0700";
        }
      ];

      # Forge's firewall only opens 22, so the port must be opened explicitly.
      # The server runs with --no-auth: opening it to the whole LAN would let any
      # device delete or overwrite the backups (encrypted, so unreadable, but not
      # protected from destruction). Accepting only homelab's IP is the access control.
      networking.firewall = {
        extraCommands = ''
          iptables -A nixos-fw -p tcp -s ${homelabIp} --dport ${toString port} -j nixos-fw-accept
        '';
        extraStopCommands = ''
          iptables -D nixos-fw -p tcp -s ${homelabIp} --dport ${toString port} -j nixos-fw-accept || true
        '';
      };
    };
}
