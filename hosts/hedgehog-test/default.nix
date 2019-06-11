with import ../../util;

let
  nixos = buildNixOSConfiguration { conf = ./configuration.nix; };
in {
  inherit (nixos) system;
}
