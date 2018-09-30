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

    package = pkgs.plex.overrideAttrs (_: let
      # see https://www.plex.tv/media-server-downloads/ for 64bit rpm
      version = "1.13.8.5395";
      vsnHash = "10d48da0d";
      sha1 = "pcp4xdkj5ilqwsz4nfdxg2bddradaybp";
    in {
      name = "plex-${version}";
      src = pkgs.fetchurl {
        url = "https://downloads.plex.tv/plex-media-server/${version}-${vsnHash}/plexmediaserver-${version}-${vsnHash}.x86_64.rpm";
        inherit sha1;
      };

      # TODO: remove this once https://github.com/NixOS/nixpkgs/pull/47562 is merged in.
      installPhase = let
        dataDir = "/nas/Plex/Library/Application\ Support";
      in ''
        install -d $out/usr/lib
        cp -dr --no-preserve='ownership' usr/lib/plexmediaserver $out/usr/lib/

        # Now we need to patch up the executables and libraries to work on Nix.
        # Side note: PLEASE don't put spaces in your binary names. This is stupid.
        for bin in "Plex Media Server"              \
                   "Plex DLNA Server"               \
                   "Plex Media Scanner"             \
                   "Plex Relay"                     \
                   "Plex Script Host"               \
                   "Plex Transcoder"                \
                   "Plex Tuner Service"             ; do
          patchelf --set-interpreter "${pkgs.glibc.out}/lib/ld-linux-x86-64.so.2" "$out/usr/lib/plexmediaserver/$bin"
          patchelf --set-rpath "$out/usr/lib/plexmediaserver" "$out/usr/lib/plexmediaserver/$bin"
        done

        find $out/usr/lib/plexmediaserver/Resources -type f -a -perm -0100 \
            -print -exec patchelf --set-interpreter "${pkgs.glibc.out}/lib/ld-linux-x86-64.so.2" '{}' \;

        # executables need libstdc++.so.6
        ln -s "${pkgs.stdenv.lib.makeLibraryPath [ pkgs.stdenv.cc.cc ]}/libstdc++.so.6" "$out/usr/lib/plexmediaserver/libstdc++.so.6"

        # Our next problem is the "Resources" directory in /usr/lib/plexmediaserver.
        # This is ostensibly a skeleton directory, which contains files that Plex
        # copies into its folder in /var. Unfortunately, there are some SQLite
        # databases in the directory that are opened at startup. Since these
        # database files are read-only, SQLite chokes and Plex fails to start. To
        # solve this, we keep the resources directory in the Nix store, but we
        # rename the database files and replace the originals with symlinks to
        # /var/lib/plex. Then, in the systemd unit, the base database files are
        # copied to /var/lib/plex before starting Plex.
        RSC=$out/usr/lib/plexmediaserver/Resources
        for db in "com.plexapp.plugins.library.db"; do
            mv $RSC/$db $RSC/base_$db
            ln -s "${dataDir}/.skeleton/$db" $RSC/$db
        done
      '';
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
