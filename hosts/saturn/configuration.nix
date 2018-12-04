{ pkgs, lib, ... }:

with lib;

let

  pinnedNH = import ../../external/nixos-hardware.nix;

  nasIP = "172.25.1.2";

in {
  imports = [
    ./hardware-configuration.nix

    "${pinnedNH}/common/cpu/intel"

    ../../modules/nixos

    ./home.nix
  ];

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "saturn";

  mine.users = { yl = { uid = 2000; isAdmin = true;  home = "/yl"; }; };

  mine.gnupg.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.virtualisation.docker.enable = true;
  mine.virtualisation.libvirtd.enable = true;

  mine.hardware.machine = "saturn";

  # enable iScsi with libvirtd
  nixpkgs.overlays = [
    (self: super: {
      libvirt = super.libvirt.override {
        enableIscsi = true;
      };
    })
  ];

  # start iscsid
  # TODO(high): uncomment when full VLAN network is available
  # systemd.services.iscsid = {
  #   wantedBy = [ "multi-user.target" ];
  #   before = ["libvirtd.service"];
  #   serviceConfig.ExecStart = "${getBin pkgs.openiscsi}/bin/iscsid --foreground";
  #   restartIfChanged = false;
  # };
  # systemd.services.iscsid-nas = {
  #   wantedBy = [ "multi-user.target" ];
  #   after = ["iscsid.service"];
  #   requires = ["iscsid.service"];
  #   restartIfChanged = false;
  #   serviceConfig = {
  #     Type = "oneshot";
  #     RemainAfterExit = "yes";
  #   };
  #   script = let
  #     prodIQN = "iqn.2018-11.com.nasreddine.apollo:win10";
  #     stagingIQN = "iqn.2018-11.com.nasreddine.apollo:win10.staging";
  #   in ''
  #     export PATH="$PATH:${getBin pkgs.openiscsi}/bin"
  #
  #     if ! [[ -f /etc/iscsi/initiatorname.iscsi ]]; then
  #       mkdir -p /etc/iscsi
  #       echo "InitiatorName=$(iscsi-iname)" > /etc/iscsi/initiatorname.iscsi
  #     fi
  #
  #     # run iscsi discover, this might fail and that's OK!
  #     iscsi_discovery ${nasIP} || true
  #
  #     # discover all the iSCSI defices offered by my NAS
  #     let "timeout = $(date +%s) + 60"
  #     while ! iscsiadm --mode discovery --type sendtargets --portal ${nasIP}; do
  #       if [ "$(date +%s)" -ge "$timeout" ]; then
  #         echo "iSCSI is still not up, aborting"
  #         exit 1
  #       else
  #         sleep 0.5
  #       fi
  #     done
  #
  #     # Login to the IQN
  #     if ! iscsiadm -m session | grep -q ' ${prodIQN} '; then
  #       iscsiadm -m node -T ${prodIQN} -p ${nasIP} -l
  #     fi
  #     if ! iscsiadm -m session | grep -q ' ${stagingIQN} '; then
  #       iscsiadm -m node -T ${stagingIQN} -p ${nasIP} -l
  #     fi
  #   '';
  # };

  # configure OpenSSH server to listen on the ADMIN interface
  # TODO(high): uncomment when full VLAN network is available
  # services.openssh.listenAddresses = [ { addr = "172.25.250.4"; port = 22; } ];
  # systemd.services.sshd = {
  #   after = ["network-addresses-ifcadmin.service"];
  #   requires = ["network-addresses-ifcadmin.service"];
  #   serviceConfig = {
  #     RestartSec = "5";
  #   };
  # };

  #
  # Network
  #

  # TODO(high): Remove when full VLAN network is available
  networking.interfaces = {
    enp2s0 = { useDHCP = true; };
    enp3s0 = { useDHCP = false; };
  };

  # TODO(high): For some reason, when the firewall is enabled, I can't seem to
  # connect via SSH.
  networking.firewall.enable = mkForce false;

  # disable the networkmanager on Saturn as it is really not needed since the
  # network does never change.
  networking.networkmanager.enable = false;

  # TODO(high): uncomment when full VLAN network is available
  # networking.vlans = {
  #   ifcns1 = {
  #     id = 101;
  #     interface = "enp2s0";
  #   };
  #
  #   ifcns2 = {
  #     id = 102;
  #     interface = "enp3s0";
  #   };
  #
  #   ifcadmin = {
  #     id = 250;
  #     interface = "enp3s0";
  #   };
  # };

  #networking.interfaces = {
  #  # turn off DHCP on all real interfaces, I use virtual networks.
  #  enp2s0 = { useDHCP = false; };
  #  enp3s0 = { useDHCP = false; };

  #  # The ADMIN interface
  #  ifcadmin = {
  #    useDHCP = true;
  #  };

  #  # NS1 address
  #  ifcns1 = {
  #    useDHCP = true;
  #  };

  #  # NS2 address
  #  ifcns2 = {
  #    useDHCP = true;
  #  };

  #  # NS3 address
  #  ifcns3 = {
  #    useDHCP = true;
  #  };

  #  # NS4 address
  #  ifcns4 = {
  #    useDHCP = true;
  #  };
  #};

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
