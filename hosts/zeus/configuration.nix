{ pkgs, lib, ... }:

with lib;

assert (builtins.pathExists /yl/private);


let

  pinnedNH = import ../../external/nixos-hardware.nix;

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
    preStart = ''
      if ! [[ -f /etc/iscsi/initiatorname.iscsi ]]; then
        echo "InitiatorName=$(${getBin pkgs.openiscsi}/bin/iscsi-iname)" > /etc/iscsi/initiatorname.iscsi
      fi
    '';
  };

  # start windows 10 VM
  systemd.services.libvirtd-guest-win10 = {
    after = ["libvirtd.service" "iscsid.service"];
    requires = ["libvirtd.service" "iscsid.service"];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
    };

    script = let
      xml = ./win10.xml;
    in ''
      uuid="$(${pkgs.libvirt}/bin/virsh domuuid 'win10' || true)"
        ${pkgs.libvirt}/bin/virsh define <(sed "s/UUID/$uuid/" '${xml}')
        ${pkgs.libvirt}/bin/virsh start 'win10'
    '';

    preStop = ''
      ${pkgs.libvirt}/bin/virsh shutdown 'win10'
      let "timeout = $(date +%s) + 60"
      while [ "$(${pkgs.libvirt}/bin/virsh list --name | grep --count '^win10$')" -gt 0 ]; do
        if [ "$(date +%s)" -ge "$timeout" ]; then
          # Meh, we warned it...
          ${pkgs.libvirt}/bin/virsh destroy 'win10'
        else
          # The machine is still running, let's give it some time to shut down
          sleep 0.5
        fi
      done
    '';
  };

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
