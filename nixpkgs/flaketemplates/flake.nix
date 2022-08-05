{
  description = "My personal flake templates";

  outputs = { self, ... }: {
    templates = {
      python = {
        path = ./python-mach-nix;
        description = "A white python mach-nix project";
      };
    };
  };
}
