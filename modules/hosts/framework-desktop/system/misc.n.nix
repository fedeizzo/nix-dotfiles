{

  flake.modules.nixos.framework-desktop = {
    environment.etc."comin-test".text = "Comin deployment works\n";
    time.hardwareClockInLocalTime = true;
  };
}
