{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.greenclip.enable = mkEnableOption "workstation.greenclip";

  config = mkIf config.mine.workstation.greenclip.enable {
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
