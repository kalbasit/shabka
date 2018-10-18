let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nur-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };
in
  pinned

