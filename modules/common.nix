{ config, pkgs, lib, ... }:

{
  imports = [
    ./colemak.nix
    ./nix.nix
    ./publica.nix
    ./redshift.nix
    ./users.nix
    ./yubikey.nix
  ];

  # load the overlays that we need at the very top-level
  nixpkgs.overlays = [
    (import ../overlays/nodePackages)
    (import ../overlays/neovim)
    (import ../overlays/rbrowser)
    (self: super: { i3-config = super.i3-config.override { hostname = config.networking.hostname; }; })
  ];

  # put /tmp on tmpfs
  boot.tmpOnTmpfs = true;

  # allow unfree software on all machines
  nixpkgs.config.allowUnfree = true;

  # set the BROWSER to my rbrowser
  environment.variables.BROWSER = "${pkgs.rbrowser}/bin/rbrowser";

  # set the EDITOR to neovim
  environment.variables.EDITOR = "nvim";

  # install all completions libraries for system packages
  environment.pathsToLink = [ "/share/zsh" ];

  # set the default locale to en_US.UTF-8
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Enable the network manager, it makes life easier
  networking.networkmanager.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # setup the fonts
  fonts.fonts = with pkgs; [
    powerline-fonts
  ];

  # disable IPv6, it's not working within my network. I have to add support for
  # IPv6 inside my network before being able to enable it.
  networking.enableIPv6 = false;

  # install some basic package system-wide, The day-to-day binaries are located
  # under the overlays/all overlay.
  environment.systemPackages = with pkgs; [
    git
    curl
    neovim
  ];

  # Enable GnuPG support
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableBrowserSocket = true;

  # Enable docker support
  virtualisation.docker.enable = true;

  # Enable Virtualbox support
  virtualisation.virtualbox.host.enable = true;

  # Synchronise the clock with NTP
  services.ntp.enable = true;

  # The power button should trigger suspend
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
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

  # allow Mosh server in
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];

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
  system.stateVersion = "18.03"; # Did you read the comment?
}
