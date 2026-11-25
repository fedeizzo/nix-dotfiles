# Alias module: full pi coding agent setup (core + heavy sandbox tools).
# Import pi-core alone to skip the heavy tools (codegraph, semble).
{
  flake.modules.homeManager.pi = { inputs, ... }: {
    imports = with inputs.self.modules.homeManager; [
      pi-core
      pi-heavy
    ];
  };
}
