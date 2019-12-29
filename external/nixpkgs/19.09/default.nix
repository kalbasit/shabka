{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # See https://github.com/NixOS/nixpkgs/pull/76622
    ./0001-flashplayer-32.0.0.293-32.0.0.303.patch
    # See https://github.com/NixOS/nixpkgs/pull/70776
    ./0001-termite-passthru-vte-ng-70776.patch
    # See https://github.com/NixOS/nixpkgs/pull/74260
    # and https://github.com/NixOS/nixpkgs/issues/74916
    ./0001-virtualbox-6.0.12-6.0.14.patch
    ./0002-virtualbox-6.0.12-6.0.14.patch
    ./0003-virtualbox-6.0.12-6.0.14.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-19-09";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
