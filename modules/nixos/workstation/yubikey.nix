{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubico-piv-tool
    yubikey-manager
    yubikey-neo-manager
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
  ];

  hardware.u2f.enable = true;

  services.pcscd.enable = true;

  security.pam.u2f = {
    enable = true;
    cue = true;
  };
}
