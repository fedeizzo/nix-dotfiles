{
  flake.modules.nixos.homelab = { pkgs, ... }: {
    time.hardwareClockInLocalTime = true;

    nixpkgs.config.permittedInsecurePackages = [
      "aspnetcore-runtime-6.0.36"
      "aspnetcore-runtime-wrapped-6.0.36"
      "dotnet-sdk-6.0.428"
      "dotnet-sdk-wrapped-6.0.428"
    ];

    environment.systemPackages = with pkgs; [
      dnsmasq
      hostapd
    ];

    environment.shellAliases = {
      "update_docker_images" = ''docker images --format "{{.Repository}}:{{.Tag}}" | xargs -L1 docker pull'';
      "restart_docker_containers" = "systemctl restart docker-*.service";
    };

    system.stateVersion = "24.11";
  };
}
