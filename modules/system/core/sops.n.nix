{
  flake.modules.nixos.sops = { inputs, pkgs, ... }: {
    imports = [ inputs.sops-nix.nixosModules.sops ];

    environment.systemPackages = with pkgs; [ sops age ];

    sops = {
      defaultSopsFormat = "yaml";
      age = {
        keyFile = "/var/lib/sops/keys.txt";
        generateKey = false;
        sshKeyPaths = [ ];
      };
    };
  };
}
