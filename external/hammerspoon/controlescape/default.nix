{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [];

  patched = mkExternal {
    inherit src patches;

    name = "hammerspoon-controlescape";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
}
