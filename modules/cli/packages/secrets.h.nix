{
  flake.modules.homeManager.cli-secrets = { pkgs, lib, ... }: {
    home.packages = with pkgs; [
      rbw
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      bitwarden-cli
      yubioath-flutter
    ];
  };
}
