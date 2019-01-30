let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixos-hardware-version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };
in
  pinned
