with import ../../util;

{
  saturn = buildNixOSConfiguration { conf = ./configuration.nix; };
}
