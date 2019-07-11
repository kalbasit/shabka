{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  options.shabka.pijul.enable = mkEnableOption "pijul";

  config = mkIf config.shabka.pijul.enable {
    home.packages = with pkgs; [
      shabka.external.nixpkgs.release-unstable.pijul
    ];
  };
}
