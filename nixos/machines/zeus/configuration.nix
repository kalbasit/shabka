{ config, pkgs, lib, ... }:

{
  # Include the results of the hardware scan.
  imports = [
    <nixos-hardware/common/cpu/intel>
    <nixos-hardware/common/pc/laptop>
    <nixos-hardware/common/pc/laptop/ssd>

    ./hardware-configuration.nix

    ../../cfg/common.nix
    ../../cfg/redshift.nix
    ../../cfg/serial_console.nix
    ../../cfg/virtualisation.nix

    ../../cfg/snapper.nix
  ];

  # boot the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Define your hostname.
  networking.hostName = "zeus";

  # select a console font
  i18n.consoleFont = "Lat2-Terminus16";
  boot.earlyVconsoleSetup = true;

  # put /tmp on tmpfs
  boot.tmpOnTmpfs = true;

  # List services that you want to enable:

  # enable nix-serve
  services.nix-serve = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/zeus.nasreddine.com.key) {
    enable = true;
    secretKeyFile = "/private/network-secrets/nix/caches/zeus.nasreddine.com.key";
  };
  networking.firewall.allowedTCPPorts = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/zeus.nasreddine.com.key) [ 5000 ];

  # Enable fwupd
  services.fwupd.enable = true;

  # set the video drivers to modesetting so no other drivers are loaded
  services.xserver.videoDrivers = lib.mkForce ["modesetting"];

  # configure OpenSSH server to listen on the ADMIN interface
  services.openssh.listenAddresses = [ { addr = "172.25.250.3"; port = 22; } ];

  # Plex service
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/nas/Plex/Library/Application\ Support";

    # TODO: setup the plugins after the migration, if any.
    managePlugins = false;
    extraPlugins = [];

    package = pkgs.plex.overrideAttrs (x: let
      # see https://www.plex.tv/media-server-downloads/ for 64bit rpm
      version = "1.13.8.5339-115f087d6";
      # TODO: update to version = "1.13.8.5395-10d48da0d";
      sha1 = "7f425470387b7d6b4f31c799dc37f967cef2aae2";
    in {
      name = "plex-${version}";
      src = pkgs.fetchurl {
        url = "https://downloads.plex.tv/plex-media-server/${version}/plexmediaserver-${version}.x86_64.rpm";
        inherit sha1;
      };
    });
  };

  # Currently the Plex systemd service tries to install the skeleton directory
  # using the install command, however that command does not work when the
  # directory where the installation is happening is living on an NFS mount.
  # TODO: This will not allow me to transition the Plex plugins to Nix so I
  # must solve this and send a patch upstream to get it working nicely. Perhaps
  # a simple test before the installation should suffice.
  systemd.services.plex.preStart = lib.mkForce "true";

  #
  # Network
  #

  # disable the networkmanager on Zeus as it is really not needed since the
  # network does never change.
  networking.networkmanager.enable = lib.mkForce false;

  networking.vlans = {
    ifcns1 = {
      id = 101;
      interface = "enp2s0f0";
    };

    ifcns2 = {
      id = 102;
      interface = "enp2s0f1";
    };

    ifcns3 = {
      id = 103;
      interface = "enp4s0f0";
    };

    ifcns4 = {
      id = 104;
      interface = "enp4s0f1";
    };

    ifcadmin = {
      id = 250;
      interface = "enp0s31f6";
    };
  };

  networking.interfaces = {
    # turn off DHCP on all real interfaces, I use virtual networks.
    enp2s0f0 = { useDHCP = false; };
    enp2s0f1 = { useDHCP = false; };
    enp4s0f0 = { useDHCP = false; };
    enp4s0f1 = { useDHCP = false; };
    enp0s31f6 = { useDHCP = false; };

    # The ADMIN interface
    ifcadmin = {
      useDHCP = true;
    };

    # NS1 address
    ifcns1 = {
      useDHCP = true;
    };

    # NS2 address
    ifcns2 = {
      useDHCP = true;
    };

    # NS3 address
    ifcns3 = {
      useDHCP = true;
    };

    # NS4 address
    ifcns4 = {
      useDHCP = true;
    };
  };
}
