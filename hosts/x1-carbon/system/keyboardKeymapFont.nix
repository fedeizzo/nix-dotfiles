{ inputs, ... }:

{
  imports = [
    ../../common/system/keyboardKeymapFont.nix
    inputs.nixos-06cb-009a-fingerprint-sensor.nixosModules.open-fprintd
    inputs.nixos-06cb-009a-fingerprint-sensor.nixosModules.python-validity
  ];

  services = {
    keyd = {
      enable = true;
      keyboards = {
        default = {
          ids = [ "0001:0001" ];
          settings = {
            main = {
              capslock = "overload(control, leftcontrol)";
              leftcontrol = "capslock";
            };
          };
        };
      };
    };
    fprintd.enable = false;
    open-fprintd.enable = true;
    python-validity.enable = true;
  };

  security.pam.services = {
    doas.fprintAuth = true;
    hyprlock = {
      unixAuth = true;
      fprintAuth = true;
    };
  };
}
