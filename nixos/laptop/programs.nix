{ config, pkgs, ... }:

{
  programs.autojump.enable = true;

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
    # TODO set bash aliases
  };

  # TODO see dconf (for obinskit)
  # TODO see less command
  programs.light.enable = true;
  # TODO see ssh options

  programs.ssh.askPassword = "";
  programs.ccache.enable = true;
}
