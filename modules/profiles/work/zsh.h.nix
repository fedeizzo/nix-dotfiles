{
  flake.modules.homeManager.profile-work = { lib, ... }: {
    programs.zsh = {
      initContent = lib.mkAfter ''
        source ~/.dd-zshrc
        source ~/.sdkman-zshrc
        function prev() {
          PREV=$(fc -lrn | head -n 1)
          sh -c "pet new `printf %q "$PREV"`"
        }
        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi
      '';
    };
  };
}
