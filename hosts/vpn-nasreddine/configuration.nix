{ pkgs, lib, ... }:

assert (builtins.pathExists /yl/private);

with lib;

let
  caFile = pkgs.writeText "ca.crt" (builtins.readFile /yl/private/network-secrets/tls/pki/ca.crt);
  crtFile = pkgs.writeText "vpn.crt" (builtins.readFile /yl/private/network-secrets/vpn/server/vpn.nasreddine.com.crt);
  keyFile = pkgs.writeText "vpn.key" (builtins.readFile /yl/private/network-secrets/vpn/server/vpn.nasreddine.com.key);
  taKey = pkgs.writeText "ta.key" (builtins.readFile /yl/private/network-secrets/vpn/server/ta.key);
  dhPem = pkgs.writeText "dh.pem" (builtins.readFile /yl/private/network-secrets/vpn/server/dh.pem);

  clientConfig = pkgs.stdenvNoCC.mkDerivation rec {
    name = "nasreddine-server-client-config-dir";
    src = /yl/private/network-secrets/vpn/server/nasreddine-ccd;
    phases = ["installPhase"];
    preferLocalBuild = true;
    installPhase = ''
      cp -r $src $out
    '';
  };

in {
  networking.firewall.allowedUDPPortRanges = [ { from = 1194; to = 1194; } ];

  services.openvpn.servers = {
    server-nasreddine = {
      config = ''
        ca ${caFile}
        cert ${crtFile}
        cipher AES-128-CBC
        client-config-dir ${clientConfig}
        client-to-client
        comp-lzo
        dev tun
        dh ${dhPem}
        keepalive 10 120
        key ${keyFile}
        max-clients 20
        persist-key
        persist-tun
        port 1194
        proto udp
        tls-auth ${taKey} 0
        topology subnet
        verb 3

        # server route
        server 172.25.255.0 255.255.255.0
        route 172.25.0.0 255.255.255.0
        route 172.25.1.0 255.255.255.0
        route 172.25.2.0 255.255.255.0
        route 172.25.3.0 255.255.255.0
        route 172.25.4.0 255.255.255.0
        route 172.25.5.0 255.255.255.0
        route 172.25.10.0 255.255.255.0
        route 172.25.11.0 255.255.255.0
        route 172.25.12.0 255.255.255.0
        route 172.25.20.0 255.255.255.0
        route 172.25.250.0 255.255.255.0
        route 172.25.255.0 255.255.255.0

        # DHCP options
        push "dhcp-option DNS 172.25.10.1"
        push "dhcp-option DOMAIN nasreddine.com"
        push "dhcp-option DOMAIN remote.nasreddine.com"
        push "dhcp-option DOMAIN home.nasreddine.com"

        # all routes handled by the server (coming from clients)
        push "route 172.25.0.0 255.255.255.0"
        push "route 172.25.1.0 255.255.255.0"
        push "route 172.25.2.0 255.255.255.0"
        push "route 172.25.3.0 255.255.255.0"
        push "route 172.25.4.0 255.255.255.0"
        push "route 172.25.5.0 255.255.255.0"
        push "route 172.25.10.0 255.255.255.0"
        push "route 172.25.11.0 255.255.255.0"
        push "route 172.25.12.0 255.255.255.0"
        push "route 172.25.20.0 255.255.255.0"
        push "route 172.25.250.0 255.255.255.0"
        push "route 172.25.255.0 255.255.255.0"
      '';
    };
  };
}
