self: super:

let
  pinnedPkgs = import ../external/nixpkgs-unstable.nix {
    inherit (super) fetchpatch runCommand;
  };
in {
  unstable = import pinnedPkgs {
    config = { allowUnfree = true; };
    overlays = [];
  };
}
