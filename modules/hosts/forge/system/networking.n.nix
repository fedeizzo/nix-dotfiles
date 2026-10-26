{
  flake.modules.nixos.forge = { config, ... }: {
    networking = {
      defaultGateway = "192.168.1.254";
      nameservers = [ "1.1.1.1" ];
      interfaces.eno1 = {
        ipv4.addresses = [{
          address = "192.168.1.89";
          prefixLength = 24;
        }];
      };
      firewall = {
        enable = true;
        interfaces.eno1.allowedTCPPorts = [ 22 ];
        # llama-swap has no auth: only homelab (its peer) may reach it.
        extraCommands = ''
          iptables -A nixos-fw -p tcp -s 192.168.1.65 --dport ${toString config.services.llama-swap.port} -j nixos-fw-accept
        '';
        extraStopCommands = ''
          iptables -D nixos-fw -p tcp -s 192.168.1.65 --dport ${toString config.services.llama-swap.port} -j nixos-fw-accept || true
        '';
      };
    };
  };
}
