{}:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);
  pinned = builtins.fetchurl {
    inherit (pinnedVersion) url sha256;
  };
in
  builtins.readFile pinned
