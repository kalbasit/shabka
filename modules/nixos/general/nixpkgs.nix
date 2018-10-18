let

  pinnedKalbasit = import ../../../external/kalbasit-nur.nix;

in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = {
        kalbasit = import pinnedKalbasit { inherit pkgs; };
      };
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
