{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # TODO: This was fixed in https://github.com/NixOS/nixpkgs/pull/58632
    # Remove this patch once the PR landed in my version updates.
    ./python36Packages-jsbeautifier-disable-check.patch

  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-unstable";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = {}; overlays = []; } // { path = patched; })
