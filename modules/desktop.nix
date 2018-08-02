{ pkgs, ... }:

{
  imports = [
    ./publica.nix
    ./redshift.nix
    ./yubikey.nix
  ];

  # set the BROWSER to my rbrowser
  environment.variables.BROWSER = "${pkgs.rbrowser}/bin/rbrowser";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # The power button should trigger suspend
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';
}
