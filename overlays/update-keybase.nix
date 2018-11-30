self: super:

let

  patchedNixpkgs = import (builtins.fetchGit {
    url = "https://github.com/kalbasit/nixpkgs.git";
    rev = "68a54acb80483ce440d8715b0328e10322d93b56";
    ref = "nixpkgs_update-keybase";
  }) {
    config = {}; overlays = [];
  };

in {
  keybase = patchedNixpkgs.keybase;
  keybase-gui = patchedNixpkgs.keybase-gui;
}
