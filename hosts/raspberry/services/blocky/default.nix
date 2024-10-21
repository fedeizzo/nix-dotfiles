_:

{
  services.blocky = {
    enable = true;
    settings = {
      upstreams = {
        init = { strategy = "fast"; };
        groups = {
          default = [ "5.9.164.112" "1.1.1.1" "tcp-tls:fdns1.dismail.de:853" "https://dns.digitale-gesellschaft.ch/dns-query" ];
        };
        strategy = "parallel_best";
        timeout = "2s";
        userAgent = "custom ua";
      };
      connectIPVersion = "dual";
      blocking = {
        blackLists = {
          all = [ "https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/wildcard/pro.txt" "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/wildcard/pro.txt" "https://gitlab.com/hagezi/mirror/-/raw/main/dns-blocklists/wildcard/pro.txt" ];
        };
        clientGroupsBlock = { default = [ "all" ]; };
        blockType = "zeroIp";
        blockTTL = "6h";
        loading = {
          refreshPeriod = "4h";
          downloads = { timeout = "60s"; attempts = 5; cooldown = "10s"; };
          concurrency = 4;
          strategy = "failOnError";
          maxErrorsPerSource = 5;
        };
      };
      bootstrapDns = [ "https://1.1.1.1/dns-query" ];
      ports = { dns = 53; http = 4000; };
      log = { level = "info"; format = "text"; timestamp = true; privacy = false; };
      prometheus = {
        enable = true;
        path = "/metrics";
      };
    };
  };
}
