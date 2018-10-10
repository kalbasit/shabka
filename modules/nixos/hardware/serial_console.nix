{ config, lib, ... }:

with lib;

{
  options.mine.serial_console.enable = mkEnableOption "Enable serial console";

  config = mkIf config.mine.serial_console.enable {
    boot.kernelParams = [ "console=tty0 console=ttyS0,115200n8" ];
    boot.loader.grub.extraConfig = ''
      serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1
      terminal_input serial console
      terminal_output serial console
    '';
  };
}

