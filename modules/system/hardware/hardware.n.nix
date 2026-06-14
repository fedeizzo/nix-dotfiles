{
  flake.modules.nixos.hardware = {
    services = {
      pcscd.enable = true;
      fwupd.enable = true;
    };
  };
}
