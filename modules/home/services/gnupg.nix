{ config, lib, ... }:

with lib;

let
  cfg = config.shabka.gnupg;
in {
  options.shabka.gnupg = {
    enable = mkEnableOption "Enable GnuPG";
    defaultCacheTtl = mkOption {
      default = 1800;
      description = ''
        Default cache TTL.
      '';
    };
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;

      enableSshSupport = true;
      enableExtraSocket = true;

      defaultCacheTtl = cfg.defaultCacheTtl;
      defaultCacheTtlSsh = cfg.defaultCacheTtl / 2;
      maxCacheTtl = cfg.defaultCacheTtl * 2;
      maxCacheTtlSsh = cfg.defaultCacheTtl;
    };
  };
}
