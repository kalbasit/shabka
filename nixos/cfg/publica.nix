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

  publica_dev_ssl_ca = builtins.readFile publica_dev_ssl_ca_path;

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

  hostsPorts = {
    "api.publica.dev"      = 8080;
    "console.publica.dev"  = 7000;
    "ctrl.publica.dev"     = 8060;
    "home.publica.dev"     = 7010;
    "js.publica.dev"       = 3002;
    "prebid.publica.dev"   = 9999;
    "publica.dev"          = 3002;
    "rewriter.publica.dev" = 8061;
  };

  generateVirtualHostsSet = host: port: {
    addSSL = true;
    serverName = host;
    sslCertificate = builtins.toPath publica_dev_ssl_cert_path;
    sslCertificateKey = builtins.toPath publica_dev_ssl_key_path;

    locations = {
      "/" = {
        extraConfig = ''
          ${commonLocation}
          proxy_pass            http://127.0.0.1:${toString port};
          proxy_buffers         4 32k;
          proxy_buffer_size     32k;
        '';
      };
    };
  };

  generateVirtualHosts = host: port: pkgs.lib.nameValuePair
    (host)
    (generateVirtualHostsSet host port);

in

assert (builtins.pathExists publica_dev_ssl_cert_path);
assert (builtins.pathExists publica_dev_ssl_key_path);
assert (builtins.pathExists publica_dev_ssl_ca_path);

{
  # Add the extra hosts
  networking.extraHosts = ''
    127.0.0.1 ${builtins.concatStringsSep " " (builtins.attrNames hostsPorts)}
  '';

  # NginX configuration for publica
  services.nginx.enable = true;

  # Add Publica CA
  security.pki.certificates = [
    publica_dev_ssl_ca
    charles_ssl_cert
  ];

  # //console/server
  services.nginx.virtualHosts = pkgs.lib.mapAttrs' generateVirtualHosts hostsPorts;
}
