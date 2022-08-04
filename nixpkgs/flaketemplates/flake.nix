{
  description = "My personal flake templates";

  outputs = { self, ... }: {
    templates = {
      latex-report = {
        path = ./latex-report-template;
        description = "A latex whitepaper project";
      };
      rust-hello = {
        path = ./rust-hello-template;
        description = "Simple Hello World in Rust";
      };
    };
  };
}
