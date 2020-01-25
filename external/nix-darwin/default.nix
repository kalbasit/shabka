{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ./183-copy-fonts-instead-of-hardlink.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nix-darwin";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
}
