with import ../../util;

let
  nixos = buildNixOSConfiguration { conf = ./configuration.nix; withShim = true; };
in {
  inherit (nixos) system;
}
