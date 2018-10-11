assert (builtins.pathExists /home/kalbasit/private);

let

  pinnedNH = import ../../external/nixos-hardware.nix;

in {
  imports = [
    ./hardware-configuration.nix

    "${pinnedNH}/common/cpu/intel"
    "${pinnedNH}/common/pc/laptop"
    "${pinnedNH}/common/pc/laptop/ssd"

    ../../modules/nixos

    ./home.nix
  ];

  networking.hostName = "hades";

  mine.hardware.intel_backlight.enable = true;
  mine.openvpn.client.expressvpn.enable = true;
  mine.printing.enable = true;
  mine.tmux.enable = true;
  mine.virtualisation.docker.enable = true;
  mine.workstation.enable = true;
  mine.workstation.publica.enable = true;

  mine.hardware.machine = "precision-7530";

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
        ca /home/kalbasit/private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/ca.crt
        cert /home/kalbasit/private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/public.crt
        key /home/kalbasit/private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/home/kalbasit/private.key
        tls-auth /home/kalbasit/private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/ta.key 1
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
