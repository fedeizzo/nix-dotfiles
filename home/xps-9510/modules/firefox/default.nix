{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    profiles."fedeizzo" = {
      extraConfig = ''
        user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
        user_pref("browser.compactmode.show", true);
        user_pref("browser.disableResetPrompt", true);
        user_pref("layers.acceleration.force-enabled", true)
        user_pref("gfx.webrender.all", true)
        user_pref("gfx.webrender.enabled", true)
        user_pref("layout.css.backdrop-filter.enabled", true)
        user_pref("svg.context-properties.content.enabled", true)

        # LINUX ONLY - WORKAROUND FOR BAR HIDING ON DRAG EVENT
        user_pref("widget.gtk.ignore-bogus-leave-notify", 1)
      '';

      userChrome = (builtins.readFile ./chrome/userChrome.css);
    };
  };
}
