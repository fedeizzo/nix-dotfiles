{
  flake.modules.nixos.x1-nano = { pkgs, ... }: {
    programs = {
      gnupg.agent.enable = true;
      gnupg.agent.pinentryPackage = pkgs.pinentry-qt;
      ssh.askPassword = "";
    };

    environment.systemPackages = [ pkgs.pinentry-qt ];
  };
}
