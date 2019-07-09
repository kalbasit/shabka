{ config, lib, ... }:

with lib;

{
  options.shabka.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.shabka.gnupg.enable {
    services.gpg-agent = {
      enable = true;

      defaultCacheTtl = 68400;
      enableSshSupport = true;
      enableExtraSocket = true;
      maxCacheTtl = 68400;
    };
  };
}
