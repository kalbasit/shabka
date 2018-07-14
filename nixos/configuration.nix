# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # Include the results of the hardware scan.
  imports = [
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.editor = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # enable build on Sandbox
  nix.useSandbox = true;

  # Define your hostname.
  networking.hostName = "cratos";

  # Enable the network manager, it makes life easier
  networking.networkmanager.enable = true;

  # Add the extra hosts
  networking.extraHosts = ''
    127.0.0.1 api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev
  '';

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    defaultLocale = "en_US.UTF-8";
    consoleUseXkbConfig = true;
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # core of my tools
    git
    curl
    neovim # TODO: use the Neovim package I create in my .nixpkgs
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # enable zsh
  programs.zsh.enable = true;

  # enable Sway
  programs.sway.enable = true;

  # set the default to zsh for all users
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # define the users
  users.extraUsers.kalbasit = {
    extraGroups = [
      "docker"
      "fuse"
      "libvirtd"
      "networkmanager"
      "sway"
      "vboxusers"
      "video"
      "wheel"
    ];

    hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
    isNormalUser = true;
    uid = 1026;

    openssh.authorizedKeys.keys = [
      (builtins.readFile (builtins.fetchurl {
        url = "https://github.com/kalbasit.keys";
        sha256 = "439dea6077640c229dcaa2a2849c57424b8d7731ecc3bd4fc4ca11bb1f98cde2";
      }))
    ];
  };

  # List services that you want to enable:

  services.xserver = {
    autorun = false;
    enable = false;

    autoRepeatDelay = 200;
    autoRepeatInterval = 30;

    layout = "us";

    xkbVariant = "colemak";
  };

  # Give people part of the video group access to adjust the backlight
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Support for my workflow. This can be removed once SWM v2 lands.
  services.openssh.extraConfig = ''
    Match User kalbasit
      AcceptEnv ACTIVE_PROFILE
      AcceptEnv ACTIVE_STORY
  '';

  # Enable fwupd
  services.fwupd.enable = true;

  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # TODO TODO TODO: remove this once it's all setup
  security.sudo.wheelNeedsPassword = false;

  # hide process information of other users when running non-root
  security.hideProcessInformation = true;

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

    # Nasreddine located at https://kalbas.it/ca.crt
    ''
      -----BEGIN CERTIFICATE-----
      MIIDSjCCAjKgAwIBAgIJAPLjX3qIZ9fEMA0GCSqGSIb3DQEBCwUAMB0xGzAZBgNV
      BAMMEk5hc3JlZGRpbmUgTmV0d29yazAeFw0xNjExMjYwMjExMDFaFw0yNjExMjQw
      MjExMDFaMB0xGzAZBgNVBAMMEk5hc3JlZGRpbmUgTmV0d29yazCCASIwDQYJKoZI
      hvcNAQEBBQADggEPADCCAQoCggEBAMOmV6uIHlB2tHMukTCx5/t4TjffR2JlLF0b
      ITerg0witFZXPBprShuXDZ+5Wogc3l56jHpju+i+wgW04XYRCimi48EXIQOpIkkO
      CFLS0H2TpLQ+0TDnz0t3i3KqGnS7FQt2fL5Rf9HH4aSrvk6nu0GcINuk8Dy3yQHH
      YngzcjEBaUSvcXbn+KU7YkEg9diJAwq4ncqASXjWwDWIrgoLp0jYAngPQshuIMGS
      IyYvANKep3y/LWmREVaHh/Lpi4wgQMJfX5ZMftbKkKfE/A/WsKNhK+cqxD9LQTx2
      eXdN8BH3N+lusAlIwgVqKgQMx/rDq5HP2J5u+orZJRaPDiaRhKcCAwEAAaOBjDCB
      iTAdBgNVHQ4EFgQU6tqboS81tzV+gUYD64j0hUEPpa4wTQYDVR0jBEYwRIAU6tqb
      oS81tzV+gUYD64j0hUEPpa6hIaQfMB0xGzAZBgNVBAMMEk5hc3JlZGRpbmUgTmV0
      d29ya4IJAPLjX3qIZ9fEMAwGA1UdEwQFMAMBAf8wCwYDVR0PBAQDAgEGMA0GCSqG
      SIb3DQEBCwUAA4IBAQB7nI5FQ2p7A+mHpJl0cGvVS8HYNv798ZXTSdb8iw0Z8Gu5
      ZejoLii1j3NwjSFviM+KL4Hb8gmuKJdI/ptoO4w5TH5VXAsJEFOGE7184gQvSC/b
      Pxe4dJgo3zbtd7cZI/B1SP9s03DjCeCVANM1qZH4FAe56KvJShJ6zx/Tq1wyytPW
      2NuBkjLvn1/+piWAmGYD5j+96UTBS6+ufVq15y3jOhNJOtr+Bof13cQ9h0je6IMY
      51W4eAxadzyiJuLpw3Srx2IRY4NDqdAaPhQ2RuAUCdpE4z/jCb9HtUVLVpfL4meJ
      l93y0Ld4jQ4VGj4SFelUgMWeGSDI21vIuasDmJTQ
      -----END CERTIFICATE-----
    ''
  ];

  # update the system periodically to receive security updates as they become available
  system.autoUpgrade.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.nixos.stateVersion = "18.03"; # Did you read the comment?
}
