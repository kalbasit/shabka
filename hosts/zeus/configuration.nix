{ pkgs, lib, ... }:

with lib;

assert (builtins.pathExists /yl/private);


let

  pinnedNH = import ../../external/nixos-hardware.nix;

  nasIP = "172.25.1.2";

  buildWindows10 = env: let
    vmName = if env == "prod" then "win10"
      else if env == "staging" then "win10.staging"
      else abort "${env} is not supported";
  in {
    after = ["libvirtd.service" "iscsid.service"];
    requires = ["libvirtd.service" "iscsid.service"];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };

    script = let
      xml = pkgs.substituteAll {
        src = ./win10.xml;

        name = "${vmName}";

        mac_address = if env == "prod" then "52:54:00:54:35:95"
        else if env == "staging" then "02:68:b3:29:da:98"
        else abort "${env} is not supported";

        dev_path = if env == "prod" then "/dev/disk/by-path/ip-172.25.1.2:3260-iscsi-iqn.2018-11.com.nasreddine.apollo:win10-lun-1"
        else if env == "staging" then "/dev/disk/by-path/ip-172.25.1.2:3260-iscsi-iqn.2018-11.com.nasreddine.apollo:win10.staging-lun-1"
        else abort "${env} is not supported";
      };

    in ''
      uuid="$(${getBin pkgs.libvirt}/bin/virsh domuuid '${vmName}' || true)"
        ${getBin pkgs.libvirt}/bin/virsh define <(sed "s/UUID/$uuid/" '${xml}')
        ${getBin pkgs.libvirt}/bin/virsh start '${vmName}'
    '';

    preStop = ''
      ${getBin pkgs.libvirt}/bin/virsh shutdown '${vmName}'
      let "timeout = $(date +%s) + 60"
      while [ "$(${getBin pkgs.libvirt}/bin/virsh list --name | grep --count '^${vmName}$')" -gt 0 ]; do
        if [ "$(date +%s)" -ge "$timeout" ]; then
          # Meh, we warned it...
          ${getBin pkgs.libvirt}/bin/virsh destroy '${vmName}'
        else
          # The machine is still running, let's give it some time to shut down
          sleep 0.5
        fi
      done
    '';
  };

in {
  imports = [
    ./hardware-configuration.nix

    "${pinnedNH}/common/cpu/intel"
    "${pinnedNH}/common/pc/ssd"

    ../../modules/nixos

    ./home.nix
  ];

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "zeus";

  mine.users = { yl = { uid = 2000; isAdmin = true;  home = "/yl"; }; };

  mine.gnupg.enable = true;
  mine.openvpn.client.expressvpn.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.virtualisation.docker.enable = true;
  mine.virtualisation.libvirtd.enable = true;

  mine.hardware.machine = "zeus";

  # enable iScsi with libvirtd
  nixpkgs.overlays = [
    (self: super: {
      libvirt = super.libvirt.override {
        enableIscsi = true;
      };
    })
  ];

  # start iscsid
  systemd.services.iscsid = {
    wantedBy = [ "multi-user.target" ];
    before = ["libvirtd.service"];
    serviceConfig.ExecStart = "${getBin pkgs.openiscsi}/bin/iscsid --foreground";
  };
  systemd.services.iscsid-nas = {
    wantedBy = [ "multi-user.target" ];
    after = ["iscsid.service"];
    requires = ["iscsid.service"];
    preStart = ''
      # delay the post script until iscsid has started
      sleep 30
    '';
    script = let
      prodIQN = "iqn.2018-11.com.nasreddine.apollo:win10";
      stagingIQN = "iqn.2018-11.com.nasreddine.apollo:win10.staging";
    in ''
      if ! [[ -f /etc/iscsi/initiatorname.iscsi ]]; then
        echo "InitiatorName=$(${getBin pkgs.openiscsi}/bin/iscsi-iname)" > /etc/iscsi/initiatorname.iscsi
      fi

      # discover all the iSCSI defices offered by my NAS
      ${getBin pkgs.openiscsi}/bin/iscsi_discovery ${nasIP}

      # Login to the IQN
      ${getBin pkgs.openiscsi}/bin/iscsiadm -m node -T ${prodIQN} -p ${nasIP} -l
      ${getBin pkgs.openiscsi}/bin/iscsiadm -m node -T ${stagingIQN} -p ${nasIP} -l
    '';

  };

  # start windows 10 VM
  # systemd.services.libvirtd-guest-win10 = buildWindows10 "prod";
  systemd.services.libvirtd-guest-win10-staging = buildWindows10 "staging";

  # configure OpenSSH server to listen on the ADMIN interface
  services.openssh.listenAddresses = [ { addr = "172.25.250.3"; port = 22; } ];

  mine.plex = {
    enable = true;
    dataDir = "/nas/Plex/Library/Application\ Support";
  };

  #
  # Network
  #

  # TODO(high): For some reason, when the firewall is enabled, I can't seem to
  # connect via SSH.
  networking.firewall.enable = mkForce false;

  # disable the networkmanager on Zeus as it is really not needed since the
  # network does never change.
  networking.networkmanager.enable = false;

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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
