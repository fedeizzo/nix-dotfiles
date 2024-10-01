{ username, config, ... }:

{
  imports = [
    ../../common/system/user.nix
  ];

  users.users.${username}.hashedPasswordFile = config.sops.secrets.fedeizzo-path.path;
}
