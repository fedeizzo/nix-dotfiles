{ pkgs, ... }:

let
  conventional-commit = pkgs.callPackage ./pkgs/conventional-pre-commit.nix { };
in
{
  pre-commit = {
    check.enable = true;

    settings = {
      addGcRoot = true;

      hooks = {
        # Filesystem
        check-added-large-files.enable = true;
        check-case-conflicts.enable = true; # Insensitive filesystem
        end-of-file-fixer.enable = true;
        trim-trailing-whitespace.enable = true;

        # Bash
        check-executables-have-shebangs.enable = true;
        check-shebang-scripts-are-executable.enable = true;

        # Languages
        check-json.enable = true;
        check-toml.enable = true;

        ## Nix
        deadnix.enable = true;
        nil.enable = true;
        nixpkgs-fmt.enable = true;
        statix.enable = true;

        # Misc
        pre-commit-hook-ensure-sops.enable = true; # Do not push unencrypted secrets file managed with sops
        actionlint.enable = true; # GitHub actions
        detect-private-keys.enable = true;
        ripsecrets = {
          enable = true; # Secret keys
          excludes = [
            "hosts/xps-9510/system/networking.nix"
            "hosts/raspberry/system/networking.nix"
            "hosts/x1-carbon/system/networking.nix"
          ];
        };
        typos = {
          enable = true;
          excludes = [
            "home/common/sources/lfpreview"
          ];
        };
        conventional-commit = {
          enable = true;
          name = "conventional-commit";
          description = "A pre-commit hook that checks commit messages for Conventional Commits formatting";
          package = conventional-commit;
          entry = "${conventional-commit}/bin/conventional-pre-commit";
          args = [ "--strict" "feat" "fix" "chore" "revert" "style" "docs" "build" "refactor" "test" "ci" "perf" ];
          stages = [ "commit-msg" ];
        };
      };
    };
  };
}
