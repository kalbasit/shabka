let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./kalbasit-keys-version.json);
  pinned = builtins.fetchurl {
    inherit (pinnedVersion) url sha256;
  };
in
  pinned
