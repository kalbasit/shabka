{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-18-09";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = {}; overlays = []; } // { path = patched; })
