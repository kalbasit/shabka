{ config, lib, ... }:

with lib;

{
  options.mine.gnupg.enable = mkEnableOption "Enable GnuPG";

  config = mkIf config.mine.gnupg.enable {
    services.gpg-agent = {
      enable = true;

      defaultCacheTtl = 68400;
      enableSshSupport = true;
      enableExtraSocket = true;
      maxCacheTtl = 68400;

      extraConfig = ''
        extra-socket /run/user/2000/gnupg/S.gpg-agent.extra
      '';
    };
  };
}
