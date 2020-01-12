{ lib, ... }:

with lib;

{
  options.shabka.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    type = types.enum [
      "cloud"
      "hetzner_cloud"
      "hetzner_sb36"
      "hetzner_sb53"
      "precision-7530"
      "thinkpad-e580"
      "xps-13"
      "zeus"
    ];
  };
}
