{ config, lib, ... }:

with lib;

{
  options.mine.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.mine.gnupg.enable {
    services.gpg-agent = {
      enable = true;

      defaultCacheTtl = 68400;
      enableSshSupport = true;
      maxCacheTtl = 68400;
    };
  };
}
