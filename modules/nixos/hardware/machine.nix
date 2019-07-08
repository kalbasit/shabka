{ lib, ... }:

with lib;

{
  options.mine.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    type = types.enum [
      "cloud"
      "precision-7530"
      "thinkpad-e580"
      "xps-13"
      "zeus"
    ];
  };
}
