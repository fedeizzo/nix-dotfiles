{
  flake-file.inputs.nur.url = "github:nix-community/NUR";
  flake-file.inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  flake.modules.nixos.misc = { inputs, ... }: {
    imports = [ inputs.nur.modules.nixos.default ];

    i18n.defaultLocale = "en_US.UTF-8";
    console.keyMap = "us";
    time.timeZone = "Europe/Paris";

    programs.ccache.enable = true;

    systemd.settings.Manager = {
      DefaultLimitNOFILE = "4096:524288";
    };

    virtualisation = {
      oci-containers.backend = "docker";
      podman.enable = false;
      docker = {
        enable = true;
        enableOnBoot = true;
        autoPrune = {
          enable = true;
          flags = [ ];
          dates = "weekly";
        };
      };
    };
  };
}
