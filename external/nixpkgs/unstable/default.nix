{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/NixOS/nixpkgs/pull/70776
    ./70776-expose-vte-ng-on-termite.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-unstable";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
