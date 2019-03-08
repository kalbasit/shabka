{ lib, shabka ? import <shabka> { , ... }:

with lib;

let

  nasreddineCA = builtins.readFile (builtins.fetchurl {
    url = "https://s3-us-west-1.amazonaws.com/nasreddine-infra/ca.crt";
    sha256 = "17x45njva3a535czgdp5z43gmgwl0lk68p4mgip8jclpiycb6qbl";
  });

  enableExpressVPN = builtins.pathExists /yl/private/network-secrets/vpn/client/expressvpn/auth.txt
    && builtins.pathExists /yl/private/network-secrets/vpn/client/expressvpn/ca2.crt
    && builtins.pathExists /yl/private/network-secrets/vpn/client/expressvpn/client.crt
    && builtins.pathExists /yl/private/network-secrets/vpn/client/expressvpn/client.key
    && builtins.pathExists /yl/private/network-secrets/vpn/client/expressvpn/ta.key;

  enableNasreddineVPN =  builtins.pathExists /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/ca.crt
    && builtins.pathExists /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/public.crt
    && builtins.pathExists /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/private.key
    && builtins.pathExists /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/ta.key;

in {
  imports = [
    ./hardware-configuration.nix

    "${shabka.external.nixos-hardware.path}/common/cpu/intel"
    "${shabka.external.nixos-hardware.path}/common/pc/laptop"
    "${shabka.external.nixos-hardware.path}/common/pc/laptop/ssd"

    ../../modules/nixos

    ./home.nix
  ];

  boot.tmpOnTmpfs = true;

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "hera";

  nix.buildMachines =
    if builtins.pathExists /yl/private/network-secrets/shabka/hosts/zeus/id_rsa then
    [{
      hostName = "zeus.home.nasreddine.com";
      sshUser = "builder";
      sshKey = "/yl/private/network-secrets/shabka/hosts/zeus/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    }] else [];
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  mine.hardware.intel_backlight.enable = true;
  mine.printing.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.virtualisation.docker.enable = true;

  mine.workstation = {
    enable = true;

    autorandr.enable = true;
    keeptruckin.enable = true;
  };

  mine.openvpn.client.expressvpn = mkIf enableExpressVPN {
    enable = true;
    auth_user_pass = /yl/private/network-secrets/vpn/client/expressvpn/auth.txt;
    ca             = /yl/private/network-secrets/vpn/client/expressvpn/ca2.crt;
    client_cert    = /yl/private/network-secrets/vpn/client/expressvpn/client.crt;
    client_key     = /yl/private/network-secrets/vpn/client/expressvpn/client.key;
    tls_auth       = /yl/private/network-secrets/vpn/client/expressvpn/ta.key;
  };

  mine.hardware.machine = "precision-7530";

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  security.pki.certificates = [
    nasreddineCA
  ];

  services.snapper = {
    configs = {
      "code" = {
        subvolume = "/yl/code";
      };

      "home" = {
        subvolume = "/home";
      };

      "private" = {
        subvolume = "/yl/private";
      };
    };
  };

  services.openvpn.servers = {
    client-nasreddine = mkIf enableNasreddineVPN {
      autoStart = false;

      config = ''
        client
        dev tun
        proto udp
        remote vpn.nasreddine.com 1194
        nobind
        persist-key
        persist-tun
        ca /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/ca.crt
        cert /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/public.crt
        key /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/private.key
        tls-auth /yl/private/network-secrets/vpn/client/desktop.hera.WaelNasreddine.vpn.nasreddine.com/ta.key 1
        verb 1
        cipher aes-128-cbc
        comp-lzo
      '';

      updateResolvConf = true;
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
