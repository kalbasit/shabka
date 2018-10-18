{ lib, ... }:

let

  pinnedNUR = import ../../../external/nur.nix;
  pinnedKalbasitNUR = import ../../../external/kalbasit-nur.nix;

in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = lib.recursiveUpdate
        (import pinnedNUR { inherit pkgs; })
        ({
          repos = {
            kalbasit = import pinnedKalbasitNUR { inherit pkgs; };
          };
        });
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
