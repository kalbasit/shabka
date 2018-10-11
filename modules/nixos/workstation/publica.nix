{ config, pkgs, lib, ... }:

with lib;
with import ../../../util;

let
  charles_ssl_cert_path     = /home/kalbasit/private/private-home-files/.charles/ca/charles-proxy-ssl-proxying-certificate.pem;
  publica_dev_ssl_ca_path   = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/ca.crt;
  publica_dev_ssl_cert_path = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.crt;
  publica_dev_ssl_key_path  = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.key;

  charles_ssl_cert = builtins.readFile charles_ssl_cert_path;

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
    "demo.publica.dev"     = 8124;
    "home.publica.dev"     = 7010;
    "js.publica.dev"       = 3002;
    "prebid.publica.dev"   = 9999;
    "rewriter.publica.dev" = 8061;
  };

  generateVirtualHostsSet = host: port: {
    forceSSL = true;
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

  generateVirtualHosts = host: port: nameValuePair
    (host)
    (generateVirtualHostsSet host port);

  publica_index = pkgs.writeText "index.html" ''
    <!HTML>
    <html>
      <head>
        <title>Publica - Index</title>
      </head>
      <body>
        <h1>Publica development pages.</h1>
        <ul>
          ${concatStringsSep "\n" (map (name: "<li><a href='https://${name}'>${name}</a></li>") (builtins.attrNames hostsPorts))}
        </ul>
      </body>
    </html>
  '';

  publica_index_drv = pkgs.stdenvNoCC.mkDerivation rec {
    name = "publica-index-html";
    src = publica_index;
    phases = [ "generatePublicaIndex" ];

    generatePublicaIndex = ''
      install -Dm644 ${publica_index} $out/index.html
    '';
  };

in

assert assertMsg (builtins.pathExists charles_ssl_cert_path) "Charles certificate was not found";
assert assertMsg (builtins.pathExists publica_dev_ssl_ca_path) "Publica CA was not found";
assert assertMsg (builtins.pathExists publica_dev_ssl_cert_path) "Publica certificate was not found";
assert assertMsg (builtins.pathExists publica_dev_ssl_key_path)  "Publica key was not found";

{
  options.mine.workstation.publica.enable = mkEnableOption "Enable Publica";

  config = mkIf config.mine.workstation.publica.enable {
    # Add the extra hosts
    networking.extraHosts = ''
      127.0.0.1 publica.dev k8s.publica.dev ${builtins.concatStringsSep " " (builtins.attrNames hostsPorts)}
    '';

    # NginX configuration for publica
    services.nginx.enable = true;

    # Add Publica CA
    security.pki.certificates = [
      publica_dev_ssl_ca
      charles_ssl_cert
    ];

    # //console/server
    services.nginx.virtualHosts = mapAttrs' generateVirtualHosts hostsPorts // {
      "publica.dev" = {
        forceSSL = true;
        serverName = "publica.dev";
        sslCertificate = builtins.toPath publica_dev_ssl_cert_path;
        sslCertificateKey = builtins.toPath publica_dev_ssl_key_path;

        locations = {
          "/" = {
            index = "index.html";
            root = publica_index_drv;
          };
        };
      };
    };
  };
}
