{ ... }:

assert (builtins.pathExists /private);

{
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
}
