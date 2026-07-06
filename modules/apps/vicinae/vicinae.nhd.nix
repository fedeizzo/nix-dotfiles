{
  flake-file.inputs.vicinae.url = "github:vicinaehq/vicinae";
  flake-file.inputs.vicinae-extensions.url = "github:vicinaehq/extensions";

  flake.modules.homeManager.vicinae = { inputs, pkgs, ... }: {
    imports = [
      inputs.vicinae.homeManagerModules.default
    ];

    programs.vicinae = {
      enable = true;
      systemd.enable = pkgs.stdenv.isLinux;
      extensions = (with inputs.vicinae-extensions.packages.${pkgs.stdenv.hostPlatform.system}; [
        nix
        zed-recents
      ]) ++ [
        (inputs.vicinae.lib.${pkgs.stdenv.hostPlatform.system}.mkVicinaeExtension {
          pname = "vicinae-extension-sdp";
          version = "0";
          src = ./extensions/sdp;
        })
        (inputs.vicinae.lib.${pkgs.stdenv.hostPlatform.system}.mkVicinaeExtension {
          pname = "vicinae-extension-dd-prs";
          version = "0";
          src = ./extensions/dd-prs;
        })
      ];

      settings = {
        telemetry.system_info = false;
        theme.dark.name = "catppuccin-macchiato";
        providers.scripts.preferences.customDirs = [
          "~/.config/vicinae/scripts"
        ];
      };
    };

    xdg.configFile."vicinae/scripts".source = ./scripts;
  };

  flake.modules.darwin.vicinae = { self, config, pkgs, username, ... }: {
    home-manager.users.${username}.imports = [
      self.modules.homeManager.vicinae
    ];

    launchd.user.agents.vicinae = {
      command = "${config.home-manager.users.${username}.programs.vicinae.package}/bin/vicinae server";
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/tmp/vicinae.log";
        StandardErrorPath = "/tmp/vicinae.log";
      };
    };
  };
}
