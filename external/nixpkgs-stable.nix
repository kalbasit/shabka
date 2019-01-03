{}:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-stable-version.json);
  pinned = import (builtins.fetchGit {
    inherit (pinnedVersion) url rev ref;
  }) {};
in
  pinned
