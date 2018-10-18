let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./kalbasit-nur-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };
in
  pinned

