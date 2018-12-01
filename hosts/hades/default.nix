with import ../../util;

{
  hades = buildNixOSConfiguration { conf = ./configuration.nix; };
}
