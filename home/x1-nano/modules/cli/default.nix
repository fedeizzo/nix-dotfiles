{ pkgs, ... }:

{
  imports = [ ../../../common/cli/linux ];

  home.packages = [
    pkgs.gcc
  ];

  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "Federico Izzo";
        email = "federico@fedeizzo.dev";
      };

      ui = {
        paginate = "never";
      };
    };
  };
}
