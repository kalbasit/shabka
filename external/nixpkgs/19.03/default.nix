{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-19-03";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = {}; overlays = []; } // { path = patched; })
