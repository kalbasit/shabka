{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.printing.enable = mkEnableOption "Enable printing";

  config = mkIf config.mine.printing.enable {
    services.printing.enable = true;
    services.printing.drivers = with pkgs; [
      epson-escpr
      hplip
    ];

    # Allow to discover wireless printers
    services.avahi = {
      enable = true;
      nssmdns = true;
    };
  };
}
