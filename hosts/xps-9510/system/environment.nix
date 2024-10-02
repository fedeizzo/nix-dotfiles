{ pkgs, ... }:

{
  imports = [
    ../../common/system/keyboardKeymapFont.nix
  ];

  virtualisation = {
    docker = {
      enable = true;
      enableNvidia = true;
      enableOnBoot = true;
      rootless = {
        enable = true;
        setSocketVariable = true;
      };
      autoPrune = {
        enable = true;
        flags = [ ];
        dates = "weekly";
      };
    };
    libvirtd.enable = true;
  };

  environment.systemPackages = with pkgs; [
    virt-manager
  ];

}
