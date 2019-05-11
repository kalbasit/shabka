{ lib, ... }:

with lib;

{
  options.mine.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    type = types.enum [
      "cloud"
      "xps-13"
      "precision-7530"
      "zeus"
    ];
  };
}
