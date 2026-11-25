{ inputs, self, ... }:

let
  system = "aarch64-linux";
  username = "bits";

  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  pkgs-unstable = import inputs.nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };
in
{
  flake.homeConfigurations."${username}@datadog-workspace" =
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      extraSpecialArgs = {
        inherit inputs pkgs-unstable username;
      };

      modules = with self.modules.homeManager; [
        # Slim workspace: bash/web/nix LSPs only, light AI tools only,
        # core cli domains only (no desktop/media/docs/secrets).
        languages-bash
        languages-web
        languages-nix
        cli-core
        cli-net
        cli-vcs
        ai-light
        nono
        herdr
        direnv
        jujutsu
        zsh
        starship
        git
        profile-work

        {
          home = {
            inherit username;
            homeDirectory = "/home/${username}";
            sessionPath = [ "$HOME/.nix-profile/bin" ];
            stateVersion = "25.05";
          };
        }
      ];
    };
}
