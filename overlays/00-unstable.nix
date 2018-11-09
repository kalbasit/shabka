self: super:

let
  pinnedPkgs = import ../external/nixpkgs.nix { inherit (import <nixpkgs> {}) fetchpatch runCommand; };
in {
  unstable = import pinnedPkgs {
    config = {};
    overlays = []; #python3 ++ (filteredModules [] ./unstable);
  };
}
