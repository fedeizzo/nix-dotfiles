{
  flake.modules.homeManager.cli-docs = { pkgs, ... }: {
    home.packages = with pkgs; [
      pandoc
      graphviz
      pet
    ];
  };
}
