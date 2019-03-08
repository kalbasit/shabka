{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  options.mine.pijul.enable = mkEnableOption "pijul";

  config = mkIf config.mine.pijul.enable {
    home.packages = with pkgs; [
      shabka.external.nixpkgs.release-unstable.pijul
    ];
  };
}
