with import ../../util;

{
  cratos = buildNixOSConfiguration { conf = ./configuration.nix; };
}
