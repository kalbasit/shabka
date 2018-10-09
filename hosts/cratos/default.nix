assert (builtins.pathExists /private);

let

  pinnedNH = import ../../external/nixos-hardware.nix;

in {
  imports = [
    "${pinnedNH}/dell/xps/13-9360"

    ../../modules/nixos

    ./hardware-configuration.nix
    ./home.nix
  ];

  networking.hostName = "cratos";

  mine.hardware.intel_backlight.enable = true;
  mine.openvpn.client.expressvpn.enable = true;
  mine.printing.enable = true;
  mine.tmux.enable = true;
  mine.virtualisation.docker.enable = true;
  mine.workstation.enable = true;
  mine.workstation.publica.enable = true;

  mine.hardware.machine = "xps-13";

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
        ca /private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/ca.crt
        cert /private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/public.crt
        key /private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/private.key
        tls-auth /private/network-secrets/vpn/client/desktop.cratos.WaelNasreddine.vpn.nasreddine.com/ta.key 1
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
  system.stateVersion = "18.03"; # Did you read the comment?
}
