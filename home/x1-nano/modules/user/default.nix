{ ... }:

{
  imports = [
    ../../../common/user
  ];

  home.file.".sources" = {
    source = ../../../common/sources;
    executable = true;
    recursive = true;
  };
}
