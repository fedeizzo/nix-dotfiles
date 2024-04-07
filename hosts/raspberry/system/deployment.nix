{ inputs, ... }:

{
  imports = [
    inputs.comin.nixosModules.comin
  ];

  services.comin = {
    enable = true;
    remotes = [
      {
        name = "origin";
        url = "https://github.com/fedeizzo/nix-dotfiles.git";
        branches.testing.name = ""; # No testing branch on this remote
        branches.main.name = "master";
        poller.period = 60; # 60s
      }
    ];
  };

  system.activationScripts.restart_container.text = ''
    systemctl restart docker-*.service
  '';
}
