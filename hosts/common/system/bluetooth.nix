_:

{
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      disabledPlugins = [ "sap" ];
      input = {
        General = {
          UserspaceHID = true;
        };
      };
    };
  };

  # TODO add declarative devices configuration
}
