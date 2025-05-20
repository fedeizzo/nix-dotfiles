{ lib, ... }:

{
  imports = [
    # ../../../common/firefox # until the flake doesn't include m* processors
  ];

  # programs.zen-browser.package = lib.mkDefault null;
}
