{ config, lib, ... }:

with lib;

{
  options.mine.workstation.snapper.enable = mkEnableOption "workstation.snapper";

  config = mkIf config.mine.workstation.snapper.enable {
    services.snapper = {
      configs = {
        "code" = {
          subvolume = "/code";
        };

        "home" = {
          subvolume = "/home";
        };

        "private" = {
          subvolume = "/private";
        };
      };
    };
  };
}
