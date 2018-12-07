self: super:

let
  pinnedPkgs = import ../external/nixpkgs.nix {
    pkgs = (import <nixpkgs> {});
    inherit (import ../util) assertMsg;
  };
in {
  unstable = import pinnedPkgs {
    config = { allowUnfree = true; };
    overlays = [];
  };
}
