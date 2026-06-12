{
  flake.modules.nixos.x1-nano = {
    sops.defaultSopsFile = ./oven-secrets.yaml;
  };
}
