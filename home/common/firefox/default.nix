{ inputs, ... }:

{

  imports = [
    inputs.zen-browser.homeModules.beta
  ];
  # programs.firefox = {
  #   enable = true;
  #   profiles."fedeizzo" = {
  #     isDefault = true;
  #     extraConfig = ''
  #       user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
  #       user_pref("browser.compactmode.show", true);
  #       user_pref("browser.disableResetPrompt", true);
  #       user_pref("layers.acceleration.force-enabled", true);
  #       user_pref("gfx.webrender.all", true);
  #       user_pref("gfx.webrender.enabled", true);
  #       user_pref("layout.css.backdrop-filter.enabled", true);
  #       user_pref("svg.context-properties.content.enabled", true);

  #       # LINUX ONLY - WORKAROUND FOR BAR HIDING ON DRAG EVENT
  #       user_pref("widget.gtk.ignore-bogus-leave-notify", 1);
  #     '';
  #   };
  # };

  programs.zen-browser = {
    enable = true;
    policies = {
      AutofillAddressEnabled = true;
      AutofillCreditCardEnabled = false;
      DisableAppUpdate = true;
      DisableFeedbackCommands = true;
      DisableFirefoxStudies = true;
      DisablePocket = true; # save webs for later reading
      DisableTelemetry = true;
      # DontCheckDefaultBrowser = true;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
    };
  };
}
