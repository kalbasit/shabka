{ config, pkgs, lib, ... }:

let
  publica_dev_ssl_cert_path = /code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.crt;
  publica_dev_ssl_key_path = /code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.key;
  publica_dev_ssl_ca_path = /code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/ca.crt;

  charles_ssl_cert = ''
    -----BEGIN CERTIFICATE-----
    MIIFMDCCBBigAwIBAgIGAWEBtePHMA0GCSqGSIb3DQEBCwUAMIGcMS0wKwYDVQQDDCRDaGFybGVz
    IFByb3h5IENBICgxNiBKYW4gMjAxOCwgemV1cykxJTAjBgNVBAsMHGh0dHBzOi8vY2hhcmxlc3By
    b3h5LmNvbS9zc2wxETAPBgNVBAoMCFhLNzIgTHRkMREwDwYDVQQHDAhBdWNrbGFuZDERMA8GA1UE
    CAwIQXVja2xhbmQxCzAJBgNVBAYTAk5aMB4XDTAwMDEwMTAwMDAwMFoXDTQ3MDMxNjAxMjIzM1ow
    gZwxLTArBgNVBAMMJENoYXJsZXMgUHJveHkgQ0EgKDE2IEphbiAyMDE4LCB6ZXVzKTElMCMGA1UE
    CwwcaHR0cHM6Ly9jaGFybGVzcHJveHkuY29tL3NzbDERMA8GA1UECgwIWEs3MiBMdGQxETAPBgNV
    BAcMCEF1Y2tsYW5kMREwDwYDVQQIDAhBdWNrbGFuZDELMAkGA1UEBhMCTlowggEiMA0GCSqGSIb3
    DQEBAQUAA4IBDwAwggEKAoIBAQCJhLUu2f7K3e7YLlFNS7gh95m/DTQjxJlnKWVRfiPG/yFylYsE
    KdqtCHcjPQUkF1JxJ+IaXcCKq1KaiGHZ9rn5hW3M1rdqvNcz8DC/MlU9EBvrPvNBVL45KcE4V7TW
    iePc2HtwfTsO5kbFo9G6GuDGljc09WQ0dezfHKRDxW6TmRE+kO6CIGPICaz0krCkyqxJ2DcI01Qn
    asd/thfoOQOa1fXr0jQNt/LMYMuDVUMxuw689t+DwZO8D6n4GSV/1o7q2k/LblbNfUSGao3g0nDB
    6RZ6v1Tvxgrjii1BVeTH3Nw5GjQHie1KcCE58apK/pdMge/NuI822AMLZ9FIELU1AgMBAAGjggF0
    MIIBcDAPBgNVHRMBAf8EBTADAQH/MIIBLAYJYIZIAYb4QgENBIIBHROCARlUaGlzIFJvb3QgY2Vy
    dGlmaWNhdGUgd2FzIGdlbmVyYXRlZCBieSBDaGFybGVzIFByb3h5IGZvciBTU0wgUHJveHlpbmcu
    IElmIHRoaXMgY2VydGlmaWNhdGUgaXMgcGFydCBvZiBhIGNlcnRpZmljYXRlIGNoYWluLCB0aGlz
    IG1lYW5zIHRoYXQgeW91J3JlIGJyb3dzaW5nIHRocm91Z2ggQ2hhcmxlcyBQcm94eSB3aXRoIFNT
    TCBQcm94eWluZyBlbmFibGVkIGZvciB0aGlzIHdlYnNpdGUuIFBsZWFzZSBzZWUgaHR0cDovL2No
    YXJsZXNwcm94eS5jb20vc3NsIGZvciBtb3JlIGluZm9ybWF0aW9uLjAOBgNVHQ8BAf8EBAMCAgQw
    HQYDVR0OBBYEFMF9onxAB9SxqIT9a4x5QgqkgmaTMA0GCSqGSIb3DQEBCwUAA4IBAQAAlfnUj8DN
    iaVOX+Rk/CIYfRdzbmaw08dNIlN6b+IJ1KGIGPBT0jAuTabN20EICvjBortDL9q1Kd6Y8ZVArxyr
    UC08sywAAsOUTOjGVQ9wRpASSiuBWMAK95n0t8pjX7hUZszcHgt0ML+hOMYELwJCT88Yj6VIrcmg
    2NbCNs++r+bwyfadeK3z2T7hk2LiiicSgiBkWIsQyXSG8RwLYNFh0Zl0AcEp2gWpF4ZJiPEog6t7
    8susoGQfBY2JcAsNwX+l55aRK5V+QRJFQtSu8h5GQ4EvPs0o+9Gw46zKPhUhspmMX/yNaJZbak2L
    QcEgsuRkCj1dopupWh+SHlJrAdRu
    -----END CERTIFICATE-----
  '';

  publica_dev_ssl_cert =
    if builtins.pathExists (builtins.toPath publica_dev_ssl_cert_path)
    then builtins.readFile publica_dev_ssl_cert_path
    else "";

  publica_dev_ssl_key =
    if builtins.pathExists (builtins.toPath publica_dev_ssl_key_path)
    then builtins.readFile publica_dev_ssl_key_path
    else "";

  publica_dev_ssl_ca =
    if builtins.pathExists (builtins.toPath publica_dev_ssl_ca_path)
    then builtins.readFile publica_dev_ssl_ca_path
    else "";

  setupNginx = builtins.stringLength publica_dev_ssl_cert > 0 && builtins.stringLength publica_dev_ssl_key > 0 && builtins.stringLength publica_dev_ssl_ca > 0;

  commonLocation = ''
    proxy_set_header      X-Real-IP $remote_addr;
    proxy_set_header      X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header      X-Forwarded-Proto $scheme;
    proxy_set_header      Host $http_host;
    proxy_set_header      X-NginX-Proxy true;
    proxy_read_timeout    10m;
    proxy_connect_timeout 10m;
    proxy_redirect        off;
  '';
in

{
  config = lib.mkIf setupNginx {
    # Add the extra hosts
    networking.extraHosts = ''
      127.0.0.1 k8s.publica.dev api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev demo.publica.dev
    '';

    # NginX configuration for publica
    services.nginx.enable = true;

    # Add Publica CA
    security.pki.certificates = [
      publica_dev_ssl_ca
      charles_ssl_cert
    ];

    # //console/server
    services.nginx.virtualHosts."api.publica.dev".serverName = "api.publica.dev";
    services.nginx.virtualHosts."api.publica.dev".addSSL = true;
    services.nginx.virtualHosts."api.publica.dev".sslCertificate = pkgs.writeText "api.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."api.publica.dev".sslCertificateKey = pkgs.writeText "api.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."api.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8080;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //console/ui
    services.nginx.virtualHosts."console.publica.dev".serverName = "console.publica.dev";
    services.nginx.virtualHosts."console.publica.dev".addSSL = true;
    services.nginx.virtualHosts."console.publica.dev".sslCertificate = pkgs.writeText "console.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."console.publica.dev".sslCertificateKey = pkgs.writeText "console.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."console.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:7000;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //ads/controller
    services.nginx.virtualHosts."ctrl.publica.dev".serverName = "ctrl.publica.dev";
    services.nginx.virtualHosts."ctrl.publica.dev".addSSL = true;
    services.nginx.virtualHosts."ctrl.publica.dev".sslCertificate = pkgs.writeText "ctrl.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."ctrl.publica.dev".sslCertificateKey = pkgs.writeText "ctrl.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."ctrl.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8060;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //homepage
    services.nginx.virtualHosts."home.publica.dev".serverName = "home.publica.dev";
    services.nginx.virtualHosts."home.publica.dev".addSSL = true;
    services.nginx.virtualHosts."home.publica.dev".sslCertificate = pkgs.writeText "home.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."home.publica.dev".sslCertificateKey = pkgs.writeText "home.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."home.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:7010;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //ads/app/src/demo
    services.nginx.virtualHosts."js.publica.dev".serverName = "js.publica.dev";
    services.nginx.virtualHosts."js.publica.dev".serverAliases = ["publica.dev"];
    services.nginx.virtualHosts."js.publica.dev".addSSL = true;
    services.nginx.virtualHosts."js.publica.dev".sslCertificate = pkgs.writeText "js.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."js.publica.dev".sslCertificateKey = pkgs.writeText "js.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."js.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:3002;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //ads/rewriter
    services.nginx.virtualHosts."rewriter.publica.dev".serverName = "rewriter.publica.dev";
    services.nginx.virtualHosts."rewriter.publica.dev".addSSL = true;
    services.nginx.virtualHosts."rewriter.publica.dev".sslCertificate = pkgs.writeText "rewriter.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."rewriter.publica.dev".sslCertificateKey = pkgs.writeText "rewriter.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."rewriter.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8061;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';

    # //demo
    services.nginx.virtualHosts."demo.publica.dev".serverName = "demo.publica.dev";
    services.nginx.virtualHosts."demo.publica.dev".addSSL = true;
    services.nginx.virtualHosts."demo.publica.dev".sslCertificate = pkgs.writeText "demo.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."demo.publica.dev".sslCertificateKey = pkgs.writeText "demo.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."demo.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8124;
      proxy_buffers         4 32k;
      proxy_buffer_size     32k;
    '';
  };
}
