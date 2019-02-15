self: super:

let
  pinnedStable = import ../external/nixpkgs-stable.nix {
    inherit (super) fetchpatch runCommand;
  };

  pinnedUnstable = import ../external/nixpkgs-unstable.nix {
    inherit (super) fetchpatch runCommand;
  };
in {
  stable = import pinnedStable  {
    config = { allowUnfree = true; };
    overlays = [];
  };

  unstable = import pinnedUnstable  {
    config = { allowUnfree = true; };
    overlays = [];
  };
}
