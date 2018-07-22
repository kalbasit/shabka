{ config, pkgs, lib, ... }:

{
  # Add the extra hosts
  networking.extraHosts = ''
    127.0.0.1 api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev
    192.168.99.100 k8s.publica.dev
  '';
}
