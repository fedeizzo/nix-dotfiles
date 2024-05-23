{ ... }:

{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      homelab = {
        hostname = "homelab";
        user = "root";
      };
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "inode/directory" = "lf.desktop";
      "text/plain" = "vim.desktop";
      "text/html" = "firefox";
      "x-scheme-handler/http" = "firefox";
      "x-scheme-handler/https" = "firefox";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.file.".sources" = {
    source = ../../sources;
    executable = true;
    recursive = true;
  };

  nix.registry = {
    fedeizzo = {
      from = {
        id = "fedeizzo";
        type = "indirect";
      };
      to = {
        owner = "fedeizzo";
        repo = "nix-dotfiles";
        type = "github";
      };
    };
  };

}
