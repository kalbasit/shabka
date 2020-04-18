{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ../19.09/0002-flashplayer-404
    # https://github.com/NixOS/nixpkgs/pull/85373
    ./0001-flashplayer-404.patch
    # https://github.com/NixOS/nixpkgs/pull/82693
    ./0002-virtualbox.patch
    ./0003-virtualbox.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-20-03";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
