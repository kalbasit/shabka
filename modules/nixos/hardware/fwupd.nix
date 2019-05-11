{ config, lib, ... }:

{
  services.fwupd.enable = lib.optionlString (config.mine.hardware.machine != "cloud") true;
}
