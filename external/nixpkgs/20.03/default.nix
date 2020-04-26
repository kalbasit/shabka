{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ./0001-pyside-as-non-broken.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-20-03";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
