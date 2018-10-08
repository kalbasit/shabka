{ ... }:

{
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
}

