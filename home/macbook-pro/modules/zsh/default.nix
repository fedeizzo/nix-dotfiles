{ pkgs, ... }:

{
  programs.starship.enableZshIntegration = true;
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableVteIntegration = true;
    syntaxHighlighting.enable = true;
    dotDir = ".config/zsh";
    history = {
      # extented = true;
      ignoreSpace = true;
      save = 10000;
      size = 10000;
    };
    historySubstringSearch.enable = true;
    initExtraFirst = ''
      source ~/.dd-zshrc
      source ~/.sdkman-zshrc
    '';
    envExtra = ''
      . "$HOME/.cargo/env"
    '';
    shellAliases = {
      ls = "eza --icons --sort=type";
      ll = "eza -l --icons --sort=type";
      lll = "eza -l --icons --sort=type | less";
      lla = "eza -la --icons --sort=type";
      llt = "eza -T --icons --sort=type";
      cat = "bat";
      gs = "git status";
      ga = "git add -A";
      gc = "git commit -m";
      gp = "git push";
      find = "fd";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "aliases"
      ];
    };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}

