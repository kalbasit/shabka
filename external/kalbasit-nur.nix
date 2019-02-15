let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./kalbasit-nur-version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };
in
  pinned
