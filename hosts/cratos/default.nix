with import ../../util;

rec {
  nixos = buildNixOSConfiguration { conf = ./configuration.nix; };
  home = (import ./home.nix).mine.home-manager.config {
    userName = "yl";
    uid = 2000;
    isAdmin = true;
    home = "/yl";
    nixosConfig = nixos.config;
  };
}
