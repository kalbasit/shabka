{}:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-stable-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev ref;
  };
in
  pinned
