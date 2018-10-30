# TODO(high): remove the requirement for the private files
assert (builtins.pathExists /yl/private);

let

  pinnedNH = import ../../external/nixos-hardware.nix;

  nasreddineCA = builtins.readFile (builtins.fetchurl {
    url = "https://kalbas.it/ca.crt";
    sha256 = "17x45njva3a535czgdp5z43gmgwl0lk68p4mgip8jclpiycb6qbl";
  });

in {
  imports = [
    ./hardware-configuration.nix

    "${pinnedNH}/dell/xps/13-9360"

    ../../modules/nixos

    ./home.nix
  ];

  boot.tmpOnTmpfs = true;

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "cratos";

  nix.buildMachines =
    if builtins.pathExists /yl/private/private-home-files/.ssh/personal/id_rsa then
    [{
      hostName = "zeus.home.nasreddine.com";
      sshUser = "yl";
      sshKey = "/yl/private/private-home-files/.ssh/personal/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    }] else [];

  mine.hardware.intel_backlight.enable = true;
  mine.openvpn.client.expressvpn.enable = true;
  mine.printing.enable = true;
  mine.tmux.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.virtualisation.docker.enable = true;
  mine.workstation.enable = true;
  mine.workstation.publica.enable = true;

  mine.hardware.machine = "xps-13";

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
    client-nasreddine = {
      autoStart = false;

      config = ''
        client
        dev tun
        proto udp
        remote vpn.nasreddine.com 1194
        nobind
        persist-key
        persist-tun
        ca /yl/private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/ca.crt
        cert /yl/private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/public.crt
        key /yl/private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/private.key
        tls-auth /yl/private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/ta.key 1
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
