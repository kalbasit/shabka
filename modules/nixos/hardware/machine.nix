{ lib, ... }:

with lib;

{
  options.mine.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    type = types.enum [
      "precision-7530"
      "saturn"
      "xps-13"
      "zeus"
    ];
  };
}
