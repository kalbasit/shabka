{ pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };

  nasIP = "172.25.2.2";

  buildWindows10 = env: let
    vmName = if env == "prod" then "win10"
      else if env == "staging" then "win10.staging"
      else abort "${env} is not supported";
  in {
    after = ["libvirtd.service" "iscsid.service" "iscsid-nas.service"];
    requires = ["libvirtd.service" "iscsid.service" "iscsid-nas.service"];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    restartIfChanged = false;

    script = let
      xml = pkgs.substituteAll {
        src = ./win10.xml;

        name = vmName;

        mac_address = if env == "prod" then "52:54:00:54:35:95"
        else if env == "staging" then "02:68:b3:29:da:98"
        else abort "${env} is not supported";

        dev_path = if env == "prod" then "/dev/disk/by-path/ip-${nasIP}:3260-iscsi-iqn.2018-11.com.nasreddine.apollo:win10-lun-1"
        else if env == "staging" then "/dev/disk/by-path/ip-${nasIP}:3260-iscsi-iqn.2018-11.com.nasreddine.apollo:win10.staging-lun-1"
        else abort "${env} is not supported";

        source_dev = "ifcns1";
      };

    in ''
      uuid="$(${getBin pkgs.libvirt}/bin/virsh domuuid '${vmName}' || true)"
        ${getBin pkgs.libvirt}/bin/virsh define <(sed "s/UUID/$uuid/" '${xml}')
        ${getBin pkgs.libvirt}/bin/virsh start '${vmName}'
    '';

    preStop = ''
      ${getBin pkgs.libvirt}/bin/virsh shutdown '${vmName}'
      let "timeout = $(date +%s) + 120"
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

    "${shabka.external.nixos-hardware.path}/common/cpu/intel"
    "${shabka.external.nixos-hardware.path}/common/pc/ssd"

    ../../modules/nixos

    ./home.nix
  ]
  ++ (optionals (builtins.pathExists ./../../secrets/nixos) (singleton ./../../secrets/nixos));

  # set the default locale and the timeZone
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/Los_Angeles";

  networking.hostName = "zeus";

  mine.keyboard.layouts = [ "colemak" ];
  mine.neovim.enable = true;
  mine.virtualisation.libvirtd.enable = true;

  mine.users = {
    enable = true;

    users = {
      yl = { uid = 2000; isAdmin = true;  home = "/yl"; };
    };
  };

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
    restartIfChanged = false;
  };
  systemd.services.iscsid-nas = {
    wantedBy = [ "multi-user.target" ];
    after = ["iscsid.service"];
    requires = ["iscsid.service"];
    restartIfChanged = false;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };
    script = let
      prodIQN = "iqn.2018-11.com.nasreddine.apollo:win10";
      stagingIQN = "iqn.2018-11.com.nasreddine.apollo:win10.staging";
    in ''
      export PATH="$PATH:${getBin pkgs.openiscsi}/bin"

      if ! [[ -f /etc/iscsi/initiatorname.iscsi ]]; then
        mkdir -p /etc/iscsi
        echo "InitiatorName=$(iscsi-iname)" > /etc/iscsi/initiatorname.iscsi
      fi

      # run iscsi discover, this might fail and that's OK!
      let "timeout = $(date +%s) + 60"
      while ! iscsi_discovery ${nasIP}; do
        if [ "$(date +%s)" -ge "$timeout" ]; then
          echo "unable to run iscsi_discovery, going to skip this step"
          break
        else
          sleep 0.5
        fi
      done

      # discover all the iSCSI defices offered by my NAS
      let "timeout = $(date +%s) + 60"
      while ! iscsiadm --mode discovery --type sendtargets --portal ${nasIP}; do
        if [ "$(date +%s)" -ge "$timeout" ]; then
          echo "iSCSI is still not up, aborting"
          exit 1
        else
          sleep 0.5
        fi
      done

      # Login to the IQN
      if ! iscsiadm -m session | grep -q ' ${prodIQN} '; then
        iscsiadm -m node -T ${prodIQN} -p ${nasIP} -l
      fi
      if ! iscsiadm -m session | grep -q ' ${stagingIQN} '; then
        iscsiadm -m node -T ${stagingIQN} -p ${nasIP} -l
      fi
    '';
  };

  # start windows 10 VM
  systemd.services.libvirtd-guest-win10 = buildWindows10 "prod";
  # systemd.services.libvirtd-guest-win10-staging = buildWindows10 "staging";

  # configure OpenSSH server to listen on the ADMIN interface
  services.openssh.listenAddresses = [ { addr = "172.25.250.3"; port = 22; } ];
  systemd.services.sshd = {
    after = ["network-addresses-ifcadmin.service"];
    requires = ["network-addresses-ifcadmin.service"];
    serviceConfig = {
      RestartSec = "5";
    };
  };

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
