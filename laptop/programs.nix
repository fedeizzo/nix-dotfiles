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

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [ "git" "zsh-autosuggestions" "zsh-history-substring-search" "zsh-syntax-highlighting" ];
    };
    # TODO set zsh aliases
    syntaxHighlighting.enable = true;
  };
}
