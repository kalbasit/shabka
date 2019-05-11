{ lib, ... }:

with lib;

let
  shabka = import <shabka> { };

  nasreddineCA = builtins.readFile (builtins.fetchurl {
    url = "https://s3-us-west-1.amazonaws.com/nasreddine-infra/ca.crt";
    sha256 = "17x45njva3a535czgdp5z43gmgwl0lk68p4mgip8jclpiycb6qbl";
  });

in {
  imports = [
    ../../modules/nixos

    ./home.nix
  ];

  boot.tmpOnTmpfs = true;

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "demeter";

  mine.hardware.machine = "cloud";

  mine.users = {
    yl = { uid = 2000; isAdmin = true;  home = "/yl"; };
  };

  # allow Demeter to be used as a builder
  # XXX: DRY this up with Zeus's configuration
  users.users = (if builtins.pathExists /yl/private/network-secrets/shabka/hosts/demeter/id_rsa.pub then {
    builder = {
      extraGroups = ["builders"];
      openssh.authorizedKeys.keys = [
        (builtins.readFile /yl/private/network-secrets/shabka/hosts/demeter/id_rsa.pub)
      ];
      isNormalUser = true;
    };
  } else {});

  mine.useColemakKeyboardLayout = true;
  mine.virtualisation.docker.enable = true;

  security.pki.certificates = [
    nasreddineCA
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
