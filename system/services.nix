{ config, pkgs, nixpkgs-old, ... }:

{
  services.xserver = {
    enable = false;
    autorun = false;
    desktopManager.default = null;
    displayManager.lightdm.enable = false;
    displayManager.sddm.enable = false;
    displayManager.gdm.enable = false;
    layout = "us";
    xkbVariant = "altgr-intl";
    libinput = {
      enable = true;

      touchpad = {
        tapping = true;
        horizontalScrolling = true;
        scrollMethod = "twofinger";
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };
    xkbOptions = "ctrl:swapcaps";
  };
  users.groups = {
    steamps4 = { };
  };
  services.pcscd.enable = true;
}
