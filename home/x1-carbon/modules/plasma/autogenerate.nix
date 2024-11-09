_:

{
  programs.plasma = {
    shortcuts = {
      kwin = {
        "Switch to Desktop 1" = "Meta+1";
        "Switch to Desktop 2" = "Meta+2";
        "Switch to Desktop 3" = "Meta+3";
        "Switch to Desktop 4" = "Meta+4";
        "Switch to Desktop 5" = "Meta+5";
        "Switch to Desktop 6" = "Meta+6";
        "Switch to Desktop 7" = "Meta+7";
        "Window to Desktop 1" = "Meta+!";
        "Window to Desktop 2" = "Meta+@";
        "Window to Desktop 3" = "Meta+#";
        "Window to Desktop 4" = "Meta+$";
        "Window to Desktop 5" = "Meta+%";
        "Window to Desktop 6" = "Meta+^";
        "Window to Desktop 7" = "Meta+&";
        "Window Close" = "Meta+q";
      };
      plasmashell = {
        "activate task manager entry 1" = [ ];
        "activate task manager entry 2" = [ ];
        "activate task manager entry 3" = [ ];
        "activate task manager entry 4" = [ ];
        "activate task manager entry 5" = [ ];
        "activate task manager entry 6" = [ ];
        "activate task manager entry 7" = [ ];
        "manage activities" = [ ];
      };
    };
    configFile = {
      "baloofilerc"."Basic Settings"."Indexing-Enabled" = false;
      "kded5rc"."Module-device_automounter"."autoload" = false;
      "kdeglobals"."WM"."activeBackground" = "49,54,59";
      "kdeglobals"."WM"."activeBlend" = "252,252,252";
      "kdeglobals"."WM"."activeForeground" = "252,252,252";
      "kdeglobals"."WM"."inactiveBackground" = "42,46,50";
      "kdeglobals"."WM"."inactiveBlend" = "161,169,177";
      "kdeglobals"."WM"."inactiveForeground" = "161,169,177";
      "krunnerrc"."Plugins"."baloosearchEnabled" = false;
      "krunnerrc"."Plugins"."calculatorEnabled" = false;
      "krunnerrc"."Plugins"."helprunnerEnabled" = false;
      "krunnerrc"."Plugins"."krunner_appstreamEnabled" = false;
      "krunnerrc"."Plugins"."krunner_bookmarksrunnerEnabled" = false;
      "krunnerrc"."Plugins"."krunner_charrunnerEnabled" = false;
      "krunnerrc"."Plugins"."krunner_dictionaryEnabled" = false;
      "krunnerrc"."Plugins"."krunner_katesessionsEnabled" = false;
      "krunnerrc"."Plugins"."krunner_killEnabled" = false;
      "krunnerrc"."Plugins"."krunner_konsoleprofilesEnabled" = false;
      "krunnerrc"."Plugins"."krunner_placesrunnerEnabled" = false;
      "krunnerrc"."Plugins"."krunner_plasma-desktopEnabled" = false;
      "krunnerrc"."Plugins"."krunner_powerdevilEnabled" = false;
      "krunnerrc"."Plugins"."krunner_recentdocumentsEnabled" = false;
      "krunnerrc"."Plugins"."krunner_sessionsEnabled" = false;
      "krunnerrc"."Plugins"."krunner_spellcheckEnabled" = false;
      "krunnerrc"."Plugins"."krunner_webshortcutsEnabled" = false;
      "krunnerrc"."Plugins"."locationsEnabled" = false;
      "krunnerrc"."Plugins"."org.kde.activities2Enabled" = false;
      "krunnerrc"."Plugins"."org.kde.datetimeEnabled" = false;
      "krunnerrc"."Plugins"."unitconverterEnabled" = false;
      "krunnerrc"."Plugins/Favorites"."plugins" = "krunner_services,krunner_systemsettings,krunner_shell,windows";
      "kwinrc"."Tiling"."padding" = 4;
      "kwinrc"."Tiling/c263e142-bd0a-5d77-b160-11afbcbcfbd3"."tiles" = "{\"layoutDirection\":\"horizontal\",\"tiles\":[{\"width\":0.25},{\"width\":0.5},{\"width\":0.25}]}";
      "kwinrc"."Xwayland"."Scale" = 1.10;
      "plasma-localerc"."Formats"."LANG" = "en_US.UTF-8";
    };
    dataFile = {
      "dolphin/view_properties/global/.directory"."Dolphin"."ViewMode" = 2;
    };
  };
}
