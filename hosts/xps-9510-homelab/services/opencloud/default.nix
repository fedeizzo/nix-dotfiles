{ ... }:

{
  users.users.opencloud.uid = 997; # make backup consistent across machines
  services.opencloud = {
    enable = false;
    stateDir = "/var/lib/opencloud";
  };
}
