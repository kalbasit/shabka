{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-personalization-gui
    yubikey-neo-manager
    yubioath-desktop
    yubico-piv-tool
  ];

  services.udev.packages = with pkgs; [
    libu2f-host
    yubikey-personalization
  ];

  services.pcscd.enable = true;
}
