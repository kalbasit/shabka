{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/NixOS/nixpkgs/pull/79915
    ../19.09/flashplayer-update.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-unstable";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
