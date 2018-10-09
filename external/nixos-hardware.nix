let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixos-hardware-version.json);
  pinnedNH = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };
in
  pinnedNH
