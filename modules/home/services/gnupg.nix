{ config, lib, ... }:

with lib;

let
  cfg = config.shabka.gnupg;
in {
  options.shabka.gnupg = {
    enable = mkEnableOption "Enable GnuPG";

    defaultCacheTtl = mkOption {
      default = 7200;
      description = ''
        Default cache TTL.
      '';
    };

    defaultCacheTtl = mkOption {
      default = 7200;
      description = ''
        Default SSH cache TTL.
      '';
    };
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;

      enableSshSupport = true;
      enableExtraSocket = true;

      defaultCacheTtl = cfg.defaultCacheTtl;
      maxCacheTtl = cfg.defaultCacheTtl;
      defaultCacheTtlSsh = cfg.defaultCacheTtl;
      maxCacheTtlSsh = cfg.defaultCacheTtl;
    };
  };
}