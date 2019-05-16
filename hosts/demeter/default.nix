with import ../../util;

{
  nixos = buildNixOSConfiguration { conf = ./configuration.nix; };
}
