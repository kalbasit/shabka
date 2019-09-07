{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/NixOS/nixpkgs-channels/commit/c4de0bf49289bc6b1448420dea39d7a5b0f3c374
    # This hasn't yet been merged in 19.03 (if it ever gets merged), so we manually add it here
    ./add-config-locale.patch
    # https://github.com/NixOS/nixpkgs/pull/67431
    ./flashplayer-version.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-19-03";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
