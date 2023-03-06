{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    profiles."fedeizzo" = {
      extraConfig = ''
        user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
        user_pref("browser.compactmode.show", true);
        user_pref("browser.disableResetPrompt", true);
      '';

      userChrome =
        (builtins.readFile ./chrome/includes/cascade-config.css) +
        (builtins.readFile ./chrome/includes/cascade-layout.css) +
        (builtins.readFile ./chrome/includes/cascade-config-mouse.css) +
        (builtins.readFile ./chrome/includes/cascade-responsive.css) +
        (builtins.readFile ./chrome/includes/cascade-floating-panel.css) +
        (builtins.readFile ./chrome/includes/cascade-nav-bar.css) +
        (builtins.readFile ./chrome/includes/cascade-tabs.css);
    };
  };
}
