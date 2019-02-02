let
  keysVersion = builtins.fromJSON (builtins.readFile ./kalbasit-keys-version.json);
  keys = builtins.fetchurl {
    inherit (keysVersion) url sha256;
  };
in
  keys
