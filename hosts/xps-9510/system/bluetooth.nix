{ ... }:

{
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      disabledPlugins = [ "sap" ];
    };
  };

  # TODO add declarative devices configuration
}
