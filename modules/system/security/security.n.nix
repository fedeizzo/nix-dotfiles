{
  flake.modules.nixos.security = {
    security = {
      sudo.enable = false;
      doas = {
        enable = true;
        extraRules = [
          { groups = [ "wheel" ]; keepEnv = true; persist = true; }
        ];
      };
    };
  };
}
