{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ./0001-swm-fix-mod256Hash.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "kalbasit-nur";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
}
