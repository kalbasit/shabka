{ config, ... }:

{
  services.fwupd.enable = config.shabka.hardware.machine != "cloud";
}
