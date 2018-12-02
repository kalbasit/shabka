with import ../../util;

{
  zeus = buildNixOSConfiguration { conf = ./configuration.nix; };
}
