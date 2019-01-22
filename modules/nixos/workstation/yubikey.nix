{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-personalization-gui
    yubikey-neo-manager
    yubioath-desktop
    yubico-piv-tool
  ];

  hardware.u2f.enable = true;

  services.pcscd.enable = true;

  security.pam.enableU2F = true;
}
