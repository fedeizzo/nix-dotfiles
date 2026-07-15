{
  flake.modules.nixos.books = { config, lib, pkgs, ... }: {

    # Calibre-web
    services.calibre-web = {
      enable = true;
      user = "calibre-server";
      group = "calibre-server";
      listen.port = 44533;
      listen.ip = "0.0.0.0";
      openFirewall = false;
      options = {
        calibreLibrary = "/var/lib/calibre-web";
        enableBookConversion = true;
        enableBookUploading = true;
        enableKepubify = true;
      };
    };

    users.users.calibre-server = {
      uid = 206;
      group = "calibre-server";
    };
    users.groups.calibre-server.gid = 206;

    services.fail2ban = {
      jails = {
        calibre-web-bruteforce.settings = {
          enabled = true;
          filter = "calibre-web-bruteforce";
          backend = "systemd";
          journalmatch = "_SYSTEMD_UNIT=calibre-web.service";
          port = toString config.services.calibre-web.listen.port;
          maxretry = 5;
          findtime = 600;
        };
      };
    };

    environment.etc."fail2ban/filter.d/calibre-web-bruteforce.conf".text = ''
      [Definition]
      failregex = ^.*WARN .* Login failed for user "[^"]*" IP-address: <HOST>.*$
      ignoreregex =
    '';


    services.audiobookshelf = {
      enable = true;
      port = 41823;
      user = "calibre-server";
      group = "calibre-server";
    };

    # OCI Containers
    virtualisation.oci-containers.containers = {

      "bookbridge" = {
        image = "ghcr.io/cporcellijr/bookbridge:latest";
        autoStart = true;
        volumes = [
          "/var/lib/bookbridge:/data"
          "${config.services.calibre-web.options.calibreLibrary}:/books:ro"
        ];
        environment = {
          LLM_PROVIDER = "openai_compatible";
          LLM_BASE_URL = "https://llama.fedeizzo.dev/v1";
          OLLAMA_ENABLED = "true";
          TRANSCRIPTION_PROVIDER = "whispercpp";
          WHISPER_CPP_URL = "https://llama-swap.fedeizzo.dev";
          KOSYNC_ENABLED = "true";
          KOSYNC_PORT = "52914";
          CWA_ENABLED = "true";
          CWA_SERVER = "https://calibre.fedeizzo.dev";
          CALIBRE_LIBRARY_PATH = "/books";
          ABS_SERVER = "https://audiobookshelf.fedeizzo.dev";
        };
        extraOptions = [ "--network=host" ];
      };

      "epub2audiobook" = {
        image = "ghcr.io/p0n1/epub_to_audiobook:latest";
        autoStart = true;
        environment = {
          OPENAI_BASE_URL = "https://llama.fedeizzo.dev/v1";
          OPENAI_API_KEY = "sk-dummy";
        };
        volumes = [
          "/var/lib/epub2audiobook:/app"
        ];
        ports = [ "7860:7860" ];
        cmd = [ "python3" "/app_src/main_ui.py" "--host" "0.0.0.0" "--port" "7860" ];
      };
    };

    networking.firewall.interfaces.eth0.allowedTCPPorts = [ 52914 44533 ];

    environment.systemPackages = [(pkgs.callPackage ./epub_to_audiobook.package {})];
    networking.firewall.interfaces.wg0.allowedTCPPorts = [1010];

    # Dashboard & Persistence
    fi.services = [
      {
        name = "calibre";
        port = config.services.calibre-web.listen.port;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = config.services.calibre-web.options.calibreLibrary;
            user = "calibre-server";
            group = "calibre-server";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist${config.services.calibre-web.options.calibreLibrary}"
        ];
      }
      {
        name = "audiobookshelf";
        port = config.services.audiobookshelf.port;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = "/var/lib/audiobookshelf";
            user = "calibre-server";
            group = "calibre-server";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/audiobookshelf"
        ];
      }
      {
        name = "bookbridge";
        port = 5757;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = "/var/lib/bookbridge";
            user = "root";
            group = "root";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/bookbridge"
        ];
      }
      {
        name = "epub2audiobook";
        port = 7860;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = "/var/lib/epub2audiobook";
            user = "root";
            group = "root";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/epub2audiobook"
        ];
      }
    ];
  };
}
