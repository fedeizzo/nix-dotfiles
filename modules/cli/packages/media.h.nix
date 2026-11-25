{
  flake.modules.homeManager.cli-media = { pkgs, lib, ... }: {
    home.packages = with pkgs; [
      ffmpeg
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      flac
      pamixer
      playerctl
      mpd-mpris
    ];
  };
}
