{ lib, config, pkgs, ... }:

let
  groupedServices = lib.lists.groupBy (service: toString service.isExposed) config.fi.services;

  getHost = service: (lib.strings.concatStringsSep "." ((lib.lists.optional (! (isNull service.subdomain)) service.subdomain) ++ [ "fedeizzo.dev" ]));

  exposedServices = groupedServices."1";
  notExposedServices = groupedServices."";

  exposedDomains = lib.strings.concatStringsSep " " (map getHost exposedServices);
  notExposedDomains = lib.strings.concatStringsSep " " (map getHost notExposedServices);

  dns-updater = (pkgs.writeShellScriptBin
    "dns-updater"
    ''
      # === CONFIGURATION ===
      CF_API_TOKEN=$(cat ${config.sops.secrets.dns-updater-password.path})
      ZONE_ID=$(cat ${config.sops.secrets.dns-updater-zone-id.path})
      DOMAIN="fedeizzo.dev"

      # Exposed services: set to current public IP
      EXPOSED_SUBDOMAINS="${exposedDomains}"

      # Internal services: set to static internal IP
      INTERNAL_SUBDOMAINS="${notExposedDomains}"
      INTERNAL_IP="192.168.7.1"

      get_public_ip() {
        ${lib.getExe pkgs.curl} -s https://api.ipify.org
      }

      get_record_id() {
        local name="$1"
        ${lib.getExe pkgs.curl} -s -X GET "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records?type=A&name=$name" \
          -H "Authorization: Bearer $CF_API_TOKEN" \
          -H "Content-Type: application/json" | ${lib.getExe pkgs.jq} -r '.result[0].id // empty'
      }

      update_record() {
        local record_id="$1"
        local name="$2"
        local content="$3"
        ${lib.getExe pkgs.curl} -s -X PUT "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$record_id" \
          -H "Authorization: Bearer $CF_API_TOKEN" \
          -H "Content-Type: application/json" \
          --data "{\"type\":\"A\",\"name\":\"$name\",\"content\":\"$content\",\"ttl\":120,\"proxied\":false}"
      }

      create_record() {
        local name="$1"
        local content="$2"
        ${lib.getExe pkgs.curl} -s -X POST "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records" \
          -H "Authorization: Bearer $CF_API_TOKEN" \
          -H "Content-Type: application/json" \
          --data "{\"type\":\"A\",\"name\":\"$name\",\"content\":\"$content\",\"ttl\":120,\"proxied\":false}"
      }

      set_dns_record() {
        local name="$1"
        local content="$2"
        local record_id
        record_id=$(get_record_id "$name")
        if [ -n "$record_id" ]; then
          echo "Updating $name to $content"
          update_record "$record_id" "$name" "$content"
        else
          echo "Creating $name with $content"
          create_record "$name" "$content"
        fi
      }

      # === MAIN LOGIC ===

      # Exposed services: use public IP
      PUBLIC_IP=$(get_public_ip)
      for sub in $EXPOSED_SUBDOMAINS; do
        set_dns_record "$sub" "$PUBLIC_IP"
      done

      # Internal services: use static IPs
      for sub in $INTERNAL_SUBDOMAINS; do
        [ -z "$sub" ] && continue
        set_dns_record "$sub" $INTERNAL_IP
      done
    '');
in
{
  systemd.services.dns-updater = {
    description = "dns-updater";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    restartTriggers = [
      config.sops.secrets.dns-updater-zone-id.sopsFileHash
      config.sops.secrets.dns-updater-password.sopsFileHash
      exposedDomains
      notExposedDomains
    ];

    serviceConfig = {
      User = "dns-updater";
      Group = "keys";
      DynamicUser = true;
      RuntimeDirectoryMode = "0400";
      Type = "oneshot";
      ExecStart = "${lib.getExe dns-updater}";
    };
  };

  systemd.timers.dns-updater = {
    description = "Run dns-updater";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1h";
      OnUnitInactiveSec = "1h";
    };
  };

  sops.secrets.dns-updater-password = {
    format = "yaml";
    mode = "0440";
    group = "keys";
    sopsFile = ./traefik-homelab-secrets.yaml;
  };
  sops.secrets.dns-updater-zone-id = {
    format = "yaml";
    mode = "0440";
    group = "keys";
    sopsFile = ./traefik-homelab-secrets.yaml;
  };
}
