{ config, lib, ... }:

with lib;

{
  options.mine.workstation.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.mine.workstation.gnupg.enable {
    services.gpg-agent = {
      enable = true;

      defaultCacheTtl = 68400;
      maxCacheTtl = 68400;
    };
  };
}
