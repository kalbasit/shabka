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
      xml = pkgs.writeText "libvirt-guest-win10.xml" ''
        <domain type='kvm' id='2'>
          <name>win10</name>
          <uuid>UUID</uuid>
          <memory unit='KiB'>16777216</memory>
          <currentMemory unit='KiB'>16777216</currentMemory>
          <vcpu placement='static'>8</vcpu>
          <resource>
            <partition>/machine</partition>
          </resource>
          <os>
            <type arch='x86_64' machine='pc-i440fx-3.0'>hvm</type>
            <boot dev='hd'/>
          </os>
          <features>
            <acpi/>
            <apic/>
            <hyperv>
              <relaxed state='on'/>
              <vapic state='on'/>
              <spinlocks state='on' retries='8191'/>
            </hyperv>
            <vmport state='off'/>
          </features>
          <cpu mode='custom' match='exact' check='full'>
            <model fallback='forbid'>Skylake-Client-IBRS</model>
            <vendor>Intel</vendor>
            <feature policy='require' name='ss'/>
            <feature policy='require' name='hypervisor'/>
            <feature policy='require' name='tsc_adjust'/>
            <feature policy='require' name='clflushopt'/>
            <feature policy='require' name='ssbd'/>
            <feature policy='require' name='xsaves'/>
            <feature policy='require' name='pdpe1gb'/>
            <feature policy='require' name='topoext'/>
          </cpu>
          <clock offset='localtime'>
            <timer name='rtc' tickpolicy='catchup'/>
            <timer name='pit' tickpolicy='delay'/>
            <timer name='hpet' present='no'/>
            <timer name='hypervclock' present='yes'/>
          </clock>
          <on_poweroff>destroy</on_poweroff>
          <on_reboot>restart</on_reboot>
          <on_crash>destroy</on_crash>
          <pm>
            <suspend-to-mem enabled='no'/>
            <suspend-to-disk enabled='no'/>
          </pm>
          <devices>
            <emulator>/run/libvirt/nix-emulators/qemu-kvm</emulator>
            <disk type='block' device='disk'>
              <driver name='qemu' type='raw' cache='none' io='native'/>
              <source dev='/dev/disk/by-path/ip-172.25.1.2:3260-iscsi-iqn.2018-11.com.nasreddine.apollo:win10-lun-1'/>
              <backingStore/>
              <target dev='hda' bus='ide'/>
              <alias name='ide0-0-0'/>
              <address type='drive' controller='0' bus='0' target='0' unit='0'/>
            </disk>
            <disk type='file' device='cdrom'>
              <driver name='qemu' type='raw'/>
              <backingStore/>
              <target dev='hdb' bus='ide' tray='open'/>
              <readonly/>
              <alias name='ide0-0-1'/>
              <address type='drive' controller='0' bus='0' target='0' unit='1'/>
            </disk>
            <controller type='usb' index='0' model='ich9-ehci1'>
              <alias name='usb'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x05' function='0x7'/>
            </controller>
            <controller type='usb' index='0' model='ich9-uhci1'>
              <alias name='usb'/>
              <master startport='0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x05' function='0x0' multifunction='on'/>
            </controller>
            <controller type='usb' index='0' model='ich9-uhci2'>
              <alias name='usb'/>
              <master startport='2'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x05' function='0x1'/>
            </controller>
            <controller type='usb' index='0' model='ich9-uhci3'>
              <alias name='usb'/>
              <master startport='4'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x05' function='0x2'/>
            </controller>
            <controller type='pci' index='0' model='pci-root'>
              <alias name='pci.0'/>
            </controller>
            <controller type='ide' index='0'>
              <alias name='ide'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x01' function='0x1'/>
            </controller>
            <controller type='virtio-serial' index='0'>
              <alias name='virtio-serial0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x06' function='0x0'/>
            </controller>
            <interface type='direct'>
              <mac address='52:54:00:54:35:95'/>
              <source dev='ifcns1' mode='bridge'/>
              <target dev='macvtap0'/>
              <model type='rtl8139'/>
              <alias name='net0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x03' function='0x0'/>
            </interface>
            <serial type='pty'>
              <source path='/dev/pts/4'/>
              <target type='isa-serial' port='0'>
                <model name='isa-serial'/>
              </target>
              <alias name='serial0'/>
            </serial>
            <console type='pty' tty='/dev/pts/4'>
              <source path='/dev/pts/4'/>
              <target type='serial' port='0'/>
              <alias name='serial0'/>
            </console>
            <channel type='spicevmc'>
              <target type='virtio' name='com.redhat.spice.0' state='disconnected'/>
              <alias name='channel0'/>
              <address type='virtio-serial' controller='0' bus='0' port='1'/>
            </channel>
            <input type='tablet' bus='usb'>
              <alias name='input0'/>
              <address type='usb' bus='0' port='1'/>
            </input>
            <input type='mouse' bus='ps2'>
              <alias name='input1'/>
            </input>
            <input type='keyboard' bus='ps2'>
              <alias name='input2'/>
            </input>
            <graphics type='spice' port='5900' autoport='yes' listen='127.0.0.1'>
              <listen type='address' address='127.0.0.1'/>
            </graphics>
            <sound model='ich6'>
              <alias name='sound0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x04' function='0x0'/>
            </sound>
            <video>
              <model type='qxl' ram='65536' vram='65536' vgamem='16384' heads='1' primary='yes'/>
              <alias name='video0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x02' function='0x0'/>
            </video>
            <redirdev bus='usb' type='spicevmc'>
              <alias name='redir0'/>
              <address type='usb' bus='0' port='2'/>
            </redirdev>
            <redirdev bus='usb' type='spicevmc'>
              <alias name='redir1'/>
              <address type='usb' bus='0' port='3'/>
            </redirdev>
            <memballoon model='virtio'>
              <alias name='balloon0'/>
              <address type='pci' domain='0x0000' bus='0x00' slot='0x07' function='0x0'/>
            </memballoon>
          </devices>
          <seclabel type='dynamic' model='dac' relabel='yes'>
            <label>+301:+301</label>
            <imagelabel>+301:+301</imagelabel>
          </seclabel>
        </domain>
      '';
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
