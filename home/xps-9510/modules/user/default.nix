{ ... }:

{
  imports = [
    ../../../common/user
  ];

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

  home.file.".sources" = {
    source = ../../sources;
    executable = true;
    recursive = true;
  };
}
