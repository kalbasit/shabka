{ lib, ... }:

with lib;

{
  options.mine.hardware.machine = mkOption {
    description = "The machine name (usually model).";
    type = types.str;
  };
}
