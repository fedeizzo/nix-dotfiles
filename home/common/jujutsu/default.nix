{ pkgs-unstable, ... }:

{
  programs.jujutsu = {
    enable = true;
    package = pkgs-unstable.jujutsu;
    settings = {
      user = {
        name = "Federico Izzo";
      };

      ui = {
        paginate = "never";
        default-command = "status";
     };
    };
  };
}
