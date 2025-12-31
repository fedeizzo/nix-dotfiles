{ ... }:

{
  imports = [
    ./module
  ];

  services.subtrackr = {
    enable = true;
    port = 22233;
    databasePath = "/var/lib/subtrackr/subtrackr.db";
  };
}
