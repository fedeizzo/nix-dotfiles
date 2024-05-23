{ ... }:

{
  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "Sharp Corporation 0x14D1 0x00000000";
            status = "enable";
            mode = "1920x1200@59.950Hz";
          }
        ];
      };
    };
  };
}
