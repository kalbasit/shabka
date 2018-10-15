{
  nixpkgs.config = { allowUnfree = true; };
  nixpkgs.overlays = import ../../../overlays;
}
