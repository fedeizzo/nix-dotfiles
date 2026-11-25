# Alias module: the full historical language tooling set.
# Import the per-language modules (languages-python, languages-bash, ...) directly to slim a host down.
{
  flake.modules.homeManager.languages = { inputs, ... }: {
    imports = with inputs.self.modules.homeManager; [
      languages-python
      languages-bash
      languages-c
      languages-docker
      languages-lua
      languages-web
      languages-nix
    ];
  };
}
