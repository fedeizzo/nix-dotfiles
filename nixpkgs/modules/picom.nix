{ pkgs, inputs, system, ... }:

{
  services.picom = {
    enable = false;
    package = pkgs.picom;
    fade = true;
    backend = "glx";
    fadeDelta = 5;
    fadeSteps = [ "0.05" "0.05" ];
    vSync = true;
    extraOptions = ''
      glx-no-stencil = true;
      glx-copy-from-front = false;
      glx-no-rebind-pixmap = true;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      use-ewmh-active-win = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      dbe = false;
      unredir-if-possible = false;
      focus-exclude = [ ];
      detect-transient = true;
      detect-client-leader = true;
      xrender-sync-fence = true;
    '';
  };
}
