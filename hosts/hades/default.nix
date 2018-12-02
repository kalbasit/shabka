with import ../../util;

{
  hades = buildNixOSConfiguration { conf = ./configuration.nix; };
  hades-home = (import ./home.nix).mine.home-manager.config {
    userName = "yl";
    uid = 2000;
    isAdmin = true;
    home = "/yl";
    nixosConfig = {};
  };
}
