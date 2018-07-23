{ config, pkgs, lib, ... }:

let
  publica_dev_ssl_cert_path = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.crt;
  publica_dev_ssl_key_path = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/publica.dev.key;
  publica_dev_ssl_ca_path = /home/kalbasit/code/publica/base/src/github.com/publica-project/platform/contrib/nginx/ssl/ca.crt;

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
      127.0.0.1 api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev demo.publica.dev
      192.168.99.100 k8s.publica.dev
    '';

    # NginX configuration for publica
    services.nginx.enable = true;

    # Add Publica CA
    security.pki.certificates = [
      publica_dev_ssl_ca
    ];

    # //console/server
    services.nginx.virtualHosts."api.publica.dev".serverName = "api.publica.dev";
    services.nginx.virtualHosts."api.publica.dev".addSSL = true;
    services.nginx.virtualHosts."api.publica.dev".sslCertificate = pkgs.writeText "api.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."api.publica.dev".sslCertificateKey = pkgs.writeText "api.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."api.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8080;
    '';

    # //console/ui
    services.nginx.virtualHosts."console.publica.dev".serverName = "console.publica.dev";
    services.nginx.virtualHosts."console.publica.dev".addSSL = true;
    services.nginx.virtualHosts."console.publica.dev".sslCertificate = pkgs.writeText "console.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."console.publica.dev".sslCertificateKey = pkgs.writeText "console.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."console.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:7000;
    '';

    # //ads/controller
    services.nginx.virtualHosts."ctrl.publica.dev".serverName = "ctrl.publica.dev";
    services.nginx.virtualHosts."ctrl.publica.dev".addSSL = true;
    services.nginx.virtualHosts."ctrl.publica.dev".sslCertificate = pkgs.writeText "ctrl.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."ctrl.publica.dev".sslCertificateKey = pkgs.writeText "ctrl.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."ctrl.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8060;
    '';

    # //homepage
    services.nginx.virtualHosts."home.publica.dev".serverName = "home.publica.dev";
    services.nginx.virtualHosts."home.publica.dev".addSSL = true;
    services.nginx.virtualHosts."home.publica.dev".sslCertificate = pkgs.writeText "home.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."home.publica.dev".sslCertificateKey = pkgs.writeText "home.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."home.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:7010;
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
    '';

    # //ads/rewriter
    services.nginx.virtualHosts."rewriter.publica.dev".serverName = "rewriter.publica.dev";
    services.nginx.virtualHosts."rewriter.publica.dev".addSSL = true;
    services.nginx.virtualHosts."rewriter.publica.dev".sslCertificate = pkgs.writeText "rewriter.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."rewriter.publica.dev".sslCertificateKey = pkgs.writeText "rewriter.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."rewriter.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8061;
    '';

    # //demo
    services.nginx.virtualHosts."demo.publica.dev".serverName = "demo.publica.dev";
    services.nginx.virtualHosts."demo.publica.dev".addSSL = true;
    services.nginx.virtualHosts."demo.publica.dev".sslCertificate = pkgs.writeText "demo.publica.dev.cert" publica_dev_ssl_cert;
    services.nginx.virtualHosts."demo.publica.dev".sslCertificateKey = pkgs.writeText "demo.publica.dev.cert" publica_dev_ssl_key;
    services.nginx.virtualHosts."demo.publica.dev".locations."/".extraConfig = ''
      ${commonLocation}
      proxy_pass            http://127.0.0.1:8124;
    '';
  };
}
