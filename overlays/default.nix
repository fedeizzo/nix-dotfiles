{ inputs, ... }:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  additions = import ../pkgs;
  modifications = self: super: {
    waybar = super.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    });
    # notmuch = super.notmuch.overrideAttrs (oldAttrs: {
    #   version = "0.37";
    # });
    tailscalewithnginx = super.tailscale.overrideAttrs (oldAttrs: {
      subPackages = oldAttrs.subPackages ++ [ "cmd/nginx-auth" ];
      postInstall = ''
        wrapProgram $out/bin/tailscaled --prefix PATH : ${self.lib.makeBinPath [ self.iproute2 self.iptables ]}
        wrapProgram $out/bin/tailscale --suffix PATH : ${self.lib.makeBinPath [ self.procps ]}
        wrapProgram $out/bin/nginx-auth --suffix PATH : ${self.lib.makeBinPath [ self.procps ]}
        sed -i -e "s#/usr/sbin#$out/bin#" -e "/^EnvironmentFile/d" ./cmd/tailscaled/tailscaled.service
        sed -i -e "s#/usr/sbin/tailscale.nginx-auth#$out/bin/nginx-auth#" -e "/^EnvironmentFile/d" ./cmd/nginx-auth/tailscale.nginx-auth.service
        install -D -m0444 -t $out/lib/systemd/system ./cmd/tailscaled/tailscaled.service
        install -D -m0444 -t $out/lib/systemd/system ./cmd/nginx-auth/tailscale.nginx-auth.service
        install -D -m0444 -t $out/lib/systemd/system ./cmd/nginx-auth/tailscale.nginx-auth.socket
      '';
    });
  };
in
inputs.nixpkgs.lib.composeManyExtensions [ modifications additions ]
