# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  # Include the results of the hardware scan.
  imports = [
    ./hardware-configuration.nix
    ../imports/default.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.editor = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Define your hostname.
  networking.hostName = "cratos";

  # Select internationalisation properties.
  i18n.consoleFont = "latarcyrheb-sun32";

  # List services that you want to enable:

  # scale the screen by 40%
  services.xserver.windowManager.i3.extraSessionCommands = ''
    # scale by 40%
    xrandr --output eDP1 --mode 3200x1800 --scale 0.6x0.6
  '';

  # Give people part of the video group access to adjust the backlight
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  # add my custom certificates
  security.pki.certificates = [
    # Charles proxy located at /home/kalbasit/.charles/ca/charles-proxy-ssl-proxying-certificate.pem
    ''
      -----BEGIN CERTIFICATE-----
      MIIFMDCCBBigAwIBAgIGAWEBtePHMA0GCSqGSIb3DQEBCwUAMIGcMS0wKwYDVQQDDCRDaGFybGVz
      IFByb3h5IENBICgxNiBKYW4gMjAxOCwgemV1cykxJTAjBgNVBAsMHGh0dHBzOi8vY2hhcmxlc3By
      b3h5LmNvbS9zc2wxETAPBgNVBAoMCFhLNzIgTHRkMREwDwYDVQQHDAhBdWNrbGFuZDERMA8GA1UE
      CAwIQXVja2xhbmQxCzAJBgNVBAYTAk5aMB4XDTAwMDEwMTAwMDAwMFoXDTQ3MDMxNjAxMjIzM1ow
      gZwxLTArBgNVBAMMJENoYXJsZXMgUHJveHkgQ0EgKDE2IEphbiAyMDE4LCB6ZXVzKTElMCMGA1UE
      CwwcaHR0cHM6Ly9jaGFybGVzcHJveHkuY29tL3NzbDERMA8GA1UECgwIWEs3MiBMdGQxETAPBgNV
      BAcMCEF1Y2tsYW5kMREwDwYDVQQIDAhBdWNrbGFuZDELMAkGA1UEBhMCTlowggEiMA0GCSqGSIb3
      DQEBAQUAA4IBDwAwggEKAoIBAQCJhLUu2f7K3e7YLlFNS7gh95m/DTQjxJlnKWVRfiPG/yFylYsE
      KdqtCHcjPQUkF1JxJ+IaXcCKq1KaiGHZ9rn5hW3M1rdqvNcz8DC/MlU9EBvrPvNBVL45KcE4V7TW
      iePc2HtwfTsO5kbFo9G6GuDGljc09WQ0dezfHKRDxW6TmRE+kO6CIGPICaz0krCkyqxJ2DcI01Qn
      asd/thfoOQOa1fXr0jQNt/LMYMuDVUMxuw689t+DwZO8D6n4GSV/1o7q2k/LblbNfUSGao3g0nDB
      6RZ6v1Tvxgrjii1BVeTH3Nw5GjQHie1KcCE58apK/pdMge/NuI822AMLZ9FIELU1AgMBAAGjggF0
      MIIBcDAPBgNVHRMBAf8EBTADAQH/MIIBLAYJYIZIAYb4QgENBIIBHROCARlUaGlzIFJvb3QgY2Vy
      dGlmaWNhdGUgd2FzIGdlbmVyYXRlZCBieSBDaGFybGVzIFByb3h5IGZvciBTU0wgUHJveHlpbmcu
      IElmIHRoaXMgY2VydGlmaWNhdGUgaXMgcGFydCBvZiBhIGNlcnRpZmljYXRlIGNoYWluLCB0aGlz
      IG1lYW5zIHRoYXQgeW91J3JlIGJyb3dzaW5nIHRocm91Z2ggQ2hhcmxlcyBQcm94eSB3aXRoIFNT
      TCBQcm94eWluZyBlbmFibGVkIGZvciB0aGlzIHdlYnNpdGUuIFBsZWFzZSBzZWUgaHR0cDovL2No
      YXJsZXNwcm94eS5jb20vc3NsIGZvciBtb3JlIGluZm9ybWF0aW9uLjAOBgNVHQ8BAf8EBAMCAgQw
      HQYDVR0OBBYEFMF9onxAB9SxqIT9a4x5QgqkgmaTMA0GCSqGSIb3DQEBCwUAA4IBAQAAlfnUj8DN
      iaVOX+Rk/CIYfRdzbmaw08dNIlN6b+IJ1KGIGPBT0jAuTabN20EICvjBortDL9q1Kd6Y8ZVArxyr
      UC08sywAAsOUTOjGVQ9wRpASSiuBWMAK95n0t8pjX7hUZszcHgt0ML+hOMYELwJCT88Yj6VIrcmg
      2NbCNs++r+bwyfadeK3z2T7hk2LiiicSgiBkWIsQyXSG8RwLYNFh0Zl0AcEp2gWpF4ZJiPEog6t7
      8susoGQfBY2JcAsNwX+l55aRK5V+QRJFQtSu8h5GQ4EvPs0o+9Gw46zKPhUhspmMX/yNaJZbak2L
      QcEgsuRkCj1dopupWh+SHlJrAdRu
      -----END CERTIFICATE-----
    ''
  ];
}
