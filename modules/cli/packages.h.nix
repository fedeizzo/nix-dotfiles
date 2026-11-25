# Alias module: the full historical cli package set.
# Import the domain modules (cli-core, cli-net, ...) directly to slim a host down.
{
  flake.modules.homeManager.cli-packages = { inputs, ... }: {
    imports = with inputs.self.modules.homeManager; [
      cli-core
      cli-net
      cli-vcs
      cli-secrets
      cli-docs
      cli-media
      cli-desktop
      cli-nix
    ];
  };
}
