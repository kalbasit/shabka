with import ../../util;

{
  hera = buildNixOSConfiguration { conf = ./configuration.nix; };
  hera-home = (import ./home.nix).mine.home-manager.config {
    userName = "yl";
    uid = 2000;
    isAdmin = true;
    home = "/yl";
    nixosConfig = {};
  };
}
