{ config, pkgs, shabka ? import <shabka> { inherit pkgs; }, lib, ... }:

with lib;

{
  options.mine.pijul.enable = mkEnableOption "pijul";

  config = mkIf config.mine.pijul.enable {
    home.packages = with pkgs; [
      shabka.external.nixpkgs.release-unstable.pijul
    ];
  };
}
