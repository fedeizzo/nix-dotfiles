self: super:
{
  betterlockscreen = super.betterlockscreen.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "pavanjadhaw";
      repo = "betterlockscreen";
      rev = "v4.0.3";
      sha256 = "sha256-d4uI/S7Kr8yvzc4/L0BX8+TBXb4AVNMJp4gb8uXgBwA=";
    };
  });
}
