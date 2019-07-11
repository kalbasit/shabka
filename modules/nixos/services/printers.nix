{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.printing.enable = mkEnableOption "Enable printing";

  config = mkIf config.shabka.printing.enable {
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
