{ config, pkgs, lib, ... }:

with lib;
with import ../../../util;

let
  cfg = config.mine.workstation.publica;

  charles_ssl_cert = builtins.readFile cfg.charles_ssl_cert_path;

  dev_ssl_ca = builtins.readFile cfg.dev_ssl_ca_path;

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
    "pbs.publica.dev"      = 8000;
    "js.publica.dev"       = 3002;
    "prebid.publica.dev"   = 9999;
    "rewriter.publica.dev" = 8061;
  };

  generateVirtualHostsSet = host: port: {
    forceSSL = true;
    serverName = host;
    sslCertificate = cfg.dev_ssl_cert_path;
    sslCertificateKey = cfg.dev_ssl_key_path;

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
  {
    options.mine.workstation.publica = {
      enable = mkEnableOption "Enable Publica";

      # TODO(low): options must be camelcase

      charles_ssl_cert_path = mkOption {
        type = types.path;
        defaultText = ''
          Path to the Charles SSL certificate.
        '';
      };

      dev_ssl_ca_path = mkOption {
        type = types.path;
        defaultText = ''
          Path to the development SSL certificate authority.
        '';
      };

      dev_ssl_cert_path = mkOption {
        type = types.path;
        defaultText = ''
          Path to the development SSL certificate.
        '';
      };

      dev_ssl_key_path = mkOption {
        type = types.path;
        defaultText = ''
          Path to the development SSL private key.
        '';
      };
    };

    config = {
      assertions = [
        {
          assertion = builtins.pathExists cfg.charles_ssl_cert_path;
          message = "charles_ssl_cert_path must exist";
        }

        {
          assertion = builtins.pathExists cfg.dev_ssl_ca_path;
          message = "dev_ssl_ca_path must exist";
        }

        {
          assertion = builtins.pathExists cfg.dev_ssl_cert_path;
          message = "dev_ssl_cert_path must exist";
        }

        {
          assertion = builtins.pathExists cfg.dev_ssl_key_path;
          message = "dev_ssl_key_path must exist";
        }
      ];
    } // (mkMerge [
      (mkIf cfg.enable {
        # Add the extra hosts
        networking.extraHosts = ''
          127.0.0.1 publica.dev k8s.publica.dev ${builtins.concatStringsSep " " (builtins.attrNames hostsPorts)}
        '';

        # NginX configuration for publica
        services.nginx.enable = true;

        # Add Publica CA
        security.pki.certificates = [
          dev_ssl_ca
          charles_ssl_cert
        ];

        # //console/server
        services.nginx.virtualHosts = mapAttrs' generateVirtualHosts hostsPorts // {
          "publica.dev" = {
            forceSSL = true;
            serverName = "publica.dev";
            sslCertificate = builtins.toPath cfg.dev_ssl_cert_path;
            sslCertificateKey = builtins.toPath cfg.dev_ssl_key_path;

            locations = {
              "/" = {
                index = "index.html";
                root = publica_index_drv;
              };
            };
          };
        };
      })
    ]);
  }
