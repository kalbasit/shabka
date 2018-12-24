{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.pijul.enable = mkEnableOption "pijul";

  config = mkIf config.mine.pijul.enable {
    home.packages = with pkgs; [
      pijul
    ];
  };
}
