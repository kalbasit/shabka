with import ../../util;

{
  vpn-nasreddine = buildNixOSConfiguration { conf = ./configuration.nix; withShim = true; };
}
