with import ../../util;

let
  darwin = buildNixDarwinConfiguration { conf = ./configuration.nix; };
in {
  inherit (darwin) system;
}
