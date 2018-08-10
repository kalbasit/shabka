{ pkgs, lib, ... }:

{
  imports = [
    ./xorg.nix
    ./yubikey.nix
  ];

  # set the BROWSER to my rbrowser
  environment.variables.BROWSER = "${pkgs.rbrowser}/bin/rbrowser";

  # Enable sound.
  sound.enable = lib.mkForce true;
  hardware.pulseaudio.enable = true;
}
