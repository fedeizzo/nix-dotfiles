{
  # Ops mods for the orchestrator workflow: attention toasts, the ops board, quick
  # actions and the `ops` layout. Everything is declared in herdr-app/extras.json,
  # which install-mods.py also reads directly for iterating without a generation:
  #   python3 modules/apps/herdr-app/install-mods.py --extras-only \
  #     ~/.config/herdr-app modules/profiles/work/herdr-app/extras.json
  flake.modules.homeManager.profile-work = { lib, options, ... }:
    let
      extras = lib.importJSON ./herdr-app/extras.json;
      local = lib.mapAttrs (_: path: ./herdr-app + "/${path}");
    in
    {
      # profile-work is also used where herdr-app isn't imported (the remote dev
      # workspace): contribute only when its options exist.
      config = lib.optionalAttrs (options ? fi.herdrApp) {
        fi.herdrApp = {
          extraMods = local (extras.mods or { });
          extraLayouts = local (extras.layouts or { });
          extraFloating = extras.floating or [ ];
          extraKeys = extras.keys or { };
        };
      };
    };
}
