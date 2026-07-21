{
  flake.modules.homeManager.profile-personal = { lib, ... }: {
    programs.zed-editor.userSettings = {
      ssh_connections = [
        {
          host = "mixer";
          projects = [
            { paths = [ "~/audio.cpp" "~/.config/fence" ]; }
          ];
        }
      ];
    };
  };
}
