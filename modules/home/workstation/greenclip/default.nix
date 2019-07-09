{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.greenclip.enable = mkEnableOption "workstation.greenclip";

  config = mkIf config.shabka.workstation.greenclip.enable {
    home.file.".config/greenclip.cfg".text = ''
      Config {
       maxHistoryLength = 250,
       historyPath = "~/.cache/greenclip.history",
       staticHistoryPath = "~/.cache/greenclip.staticHistory",
       imageCachePath = "/tmp/",
       usePrimarySelectionAsInput = False,
       blacklistedApps = [],
       trimSpaceFromSelection = True
      }
    '';
  };
}
