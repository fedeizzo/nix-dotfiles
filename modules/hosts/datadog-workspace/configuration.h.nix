{ inputs, self, ... }:

let
  system = "aarch64-linux";
  username = "bits";

  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [ inputs.llm-agents.overlays.shared-nixpkgs ];
  };

  pkgs-unstable = import inputs.nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
    overlays = [ inputs.llm-agents.overlays.shared-nixpkgs ];
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
        languages
        cli-packages
        ai-tools
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
