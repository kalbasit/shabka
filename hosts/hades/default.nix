assert (builtins.pathExists /private);

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
        ca /private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/ca.crt
        cert /private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/public.crt
        key /private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/private.key
        tls-auth /private/network-secrets/vpn/client/desktop.hades.WaelNasreddine.vpn.nasreddine.com/ta.key 1
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

# {
#   # Include the results of the hardware scan.
#   imports = [
#     <nixos-hardware/common/cpu/intel>
#     <nixos-hardware/common/pc/laptop>
#     <nixos-hardware/common/pc/laptop/ssd>
#
#     ./hardware-configuration.nix
#
#     ../../cfg/common.nix
#     ../../cfg/desktop.nix
#     ../../cfg/docker.nix
#     ../../cfg/redshift.nix
#     ../../cfg/virtualbox.nix
#
#     ../../cfg/printers.nix
#
#     ../../cfg/publica.nix
#
#     ../../cfg/snapper.nix
#   ] ++ (if builtins.pathExists /private then [
#     ../../cfg/openvpn/client/nasreddine/hades.nix
#   ] else []);
#
#   # Enable Tmux
#   mine.tmux.enable = true;
#
#   # Enable GnuPG support
#   mine.gnupg.enable = true;
#
#   # Enable VirtualBox and Docker virtualisation services.
#   mine.virtualisation.docker.enable = true;
#   mine.virtualisation.virtualbox.enable = true;
#
#   # boot the latest kernel
#   boot.kernelPackages = pkgs.linuxPackages_latest;
#
#   # select a console font
#   i18n.consoleFont = "Lat2-Terminus16";
#   boot.earlyVconsoleSetup = true;
#
#   # put /tmp on tmpfs
#   boot.tmpOnTmpfs = true;
#
#   # List services that you want to enable:
#
#   # enable nix-serve
#   services.nix-serve = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/hades.nasreddine.com.key) {
#     enable = true;
#     secretKeyFile = "/private/network-secrets/nix/caches/hades.nasreddine.com.key";
#   };
#   networking.firewall.allowedTCPPorts = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/hades.nasreddine.com.key) [ 5000 ];
# }
