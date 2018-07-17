{ config, pkgs, lib, ... }:

{
  imports = [
    ./colemak.nix
    ./i3.nix
    ./nix.nix
    ./publica.nix
    ./users.nix
    ./xorg.nix
    ./yubikey.nix
  ];

  # set the default locale to en_US.UTF-8
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Enable the network manager, it makes life easier
  networking.networkmanager.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    git
    curl
    neovim # TODO: use the Neovim package I create in my .nixpkgs
  ];

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

  # allow Mosh server in
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # hide process information of other users when running non-root
  security.hideProcessInformation = true;

  security.pki.certificates = [
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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.nixos.stateVersion = "18.03"; # Did you read the comment?
}
