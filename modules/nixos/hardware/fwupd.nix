{ config, ... }:

{
  services.fwupd.enable = config.mine.hardware.machine != "cloud";
}
