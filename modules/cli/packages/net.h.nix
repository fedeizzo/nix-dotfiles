{
  flake.modules.homeManager.cli-net = { pkgs, lib, ... }: {
    home.packages = with pkgs; [
      hurl
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      openssh
      openssl
      gnutls
      w3m
      rclone
    ];
  };
}
