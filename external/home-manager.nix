let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./home-manager-version.json);
  pinnedHM = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };
in
  pinnedHM
