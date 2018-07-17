{ config, pkgs, lib, ... }:

{
  # Add the extra hosts
  networking.extraHosts = ''
    127.0.0.1 api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev
  '';

  # -~ HACK ~-
  # Workaround lack of kubes by installing Aerospike, Memcache, MySQL and Redis.
  # TODO: enable this once nixos-unstable advances past dae9cf6106d
  #services.aerospike = {
  #  enable = true;
  #  extraConfig = ''
  #    namespace test {
  #      replication-factor 2
  #      memory-size 4G
  #      default-ttl 1h
  #      storage-engine memory
  #    }
  #  '';
  #};
  services.memcached.enable = true;
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };
  services.redis.enable = true;
}
