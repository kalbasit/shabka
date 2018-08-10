{ config, pkgs, lib, ... }:

{
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
}
