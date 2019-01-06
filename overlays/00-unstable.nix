self: super:

let
  pinnedPkgs = import ../external/nixpkgs-unstable.nix {
    pkgs = (import <nixpkgs> {});
    inherit (import ../util) assertMsg;
  };
in {
  unstable = import pinnedPkgs {
    config = { allowUnfree = true; };
    overlays = [];
  };
}
