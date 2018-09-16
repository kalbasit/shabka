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

  #
  # Network
  #

  # disable network manager because network on Zeus is set via Static IP
  # TODO: setup DHCP on pfsense, and setup the ports on  my new switch with the
  # appropriate VLAN, then simply enable the network manager.
  networking.networkmanager.enable = lib.mkForce false;

  networking.vlans = {
    ifcadmin = {
      id = 101;
      interface = "enp2s0f0";
    };

    ifcns1 = {
      id = 102;
      interface = "enp2s0f1";
    };

    ifcns2 = {
      id = 103;
      interface = "enp4s0f0";
    };

    ifcns3 = {
      id = 104;
      interface = "enp4s0f1";
    };

    ifcns4 = {
      id = 250;
      interface = "enp0s31f6";
    };
  };

  networking.interfaces = {
    # turn off all real interfaces, I only want virtual networks.
    # TODO: Once I change my switch, flag the port with the VLAN on the switch
    # and use DHCP here!
    enp0s31f6 = { useDHCP = false; };
    enp2s0f0 = { useDHCP = false; };
    enp2s0f1 = { useDHCP = false; };
    enp4s0f0 = { useDHCP = false; };
    enp4s0f1 = { useDHCP = false; };

    # The ADMIN interface
    ifcadmin = {
      useDHCP = true;
    };

    # NS1 address
    ifcns1 = {
      ipv4 = {
        addresses = [ { address = "172.25.1.3"; prefixLength = 24; } ];
      };
    };

    # NS2 address
    ifcns2 = {
      ipv4 = {
        addresses = [ { address = "172.25.2.3"; prefixLength = 24; } ];
      };
    };

    # NS3 address
    ifcns3 = {
      ipv4 = {
        addresses = [ { address = "172.25.3.3"; prefixLength = 24; } ];
      };
    };

    # NS4 address
    ifcns4 = {
      ipv4 = {
        addresses = [ { address = "172.25.4.3"; prefixLength = 24; } ];
      };
    };
  };
}


# docker0: flags=4099<UP,BROADCAST,MULTICAST>  mtu 1500
#         inet 172.17.0.1  netmask 255.255.0.0  broadcast 172.17.255.255
#         ether 02:42:8a:f4:d0:50  txqueuelen 0  (Ethernet)
#         RX packets 0  bytes 0 (0.0 B)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 0  bytes 0 (0.0 B)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#
# enp0s31f6: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 172.25.10.191  netmask 255.255.255.0  broadcast 172.25.10.255
#         inet6 fe80::ce63:c13b:78de:b701  prefixlen 64  scopeid 0x20<link>
#         inet6 2601:646:9300:b5ed:3851:250a:a939:401a  prefixlen 64  scopeid 0x0<global>
#         inet6 2601:646:9300:b5ed:470c:4700:6078:5095  prefixlen 64  scopeid 0x0<global>
#         ether d0:50:99:93:65:93  txqueuelen 1000  (Ethernet)
#         RX packets 3028  bytes 344336 (336.2 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 600  bytes 76722 (74.9 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 16  memory 0xdf300000-df320000
#
# enp2s0f0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 172.25.10.169  netmask 255.255.255.0  broadcast 172.25.10.255
#         inet6 fe80::e4ba:384c:e915:46df  prefixlen 64  scopeid 0x20<link>
#         inet6 2601:646:9300:b5ed:4bb6:2f2f:2dd7:e73d  prefixlen 64  scopeid 0x0<global>
#         inet6 2601:646:9300:b5ed:a1d7:20ab:46d9:fad8  prefixlen 64  scopeid 0x0<global>
#         ether 68:05:ca:3e:77:34  txqueuelen 1000  (Ethernet)
#         RX packets 2748  bytes 291061 (284.2 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 604  bytes 77394 (75.5 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 18  memory 0xdf2a0000-df2c0000
#
# enp2s0f1: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 172.25.10.110  netmask 255.255.255.0  broadcast 172.25.10.255
#         inet6 fe80::4f69:45e5:9aab:e2d  prefixlen 64  scopeid 0x20<link>
#         inet6 2601:646:9300:b5ed:841e:f92f:a0e8:bb47  prefixlen 64  scopeid 0x0<global>
#         inet6 2601:646:9300:b5ed:44c1:88d8:491d:9e31  prefixlen 64  scopeid 0x0<global>
#         ether 68:05:ca:3e:77:35  txqueuelen 1000  (Ethernet)
#         RX packets 2743  bytes 289611 (282.8 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 603  bytes 77044 (75.2 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 19  memory 0xdf240000-df260000
#
# enp4s0f0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 172.25.10.111  netmask 255.255.255.0  broadcast 172.25.10.255
#         inet6 fe80::10f6:1c4e:1d3a:d8c1  prefixlen 64  scopeid 0x20<link>
#         inet6 2601:646:9300:b5ed:570:e762:90a4:8e9b  prefixlen 64  scopeid 0x0<global>
#         inet6 2601:646:9300:b5ed:f0e0:be37:66f4:5383  prefixlen 64  scopeid 0x0<global>
#         ether 68:05:ca:3d:cf:4a  txqueuelen 1000  (Ethernet)
#         RX packets 4189  bytes 547939 (535.0 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 3210  bytes 381171 (372.2 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 18  memory 0xdf1a0000-df1c0000
#
# enp4s0f1: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
#         inet 172.25.10.112  netmask 255.255.255.0  broadcast 172.25.10.255
#         inet6 2601:646:9300:b5ed:f8a1:3c11:3f78:7b32  prefixlen 64  scopeid 0x0<global>
#         inet6 fe80::2f6d:ed8e:f350:7ef2  prefixlen 64  scopeid 0x20<link>
#         inet6 2601:646:9300:b5ed:b6ab:10fe:d20c:f9e9  prefixlen 64  scopeid 0x0<global>
#         ether 68:05:ca:3d:cf:4b  txqueuelen 1000  (Ethernet)
#         RX packets 2743  bytes 289611 (282.8 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 604  bytes 77398 (75.5 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#         device interrupt 19  memory 0xdf140000-df160000
#
# lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536
#         inet 127.0.0.1  netmask 255.0.0.0
#         inet6 ::1  prefixlen 128  scopeid 0x10<host>
#         loop  txqueuelen 1000  (Local Loopback)
#         RX packets 80  bytes 7280 (7.1 KiB)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 80  bytes 7280 (7.1 KiB)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#
# vboxnet0: flags=4099<UP,BROADCAST,MULTICAST>  mtu 1500
#         inet 192.168.56.1  netmask 255.255.255.0  broadcast 0.0.0.0
#         ether 0a:00:27:00:00:00  txqueuelen 1000  (Ethernet)
#         RX packets 0  bytes 0 (0.0 B)
#         RX errors 0  dropped 0  overruns 0  frame 0
#         TX packets 0  bytes 0 (0.0 B)
#         TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
#
