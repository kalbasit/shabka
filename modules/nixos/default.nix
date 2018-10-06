with import ../../util;

{
  imports = recImport ./. ++ [../../overlays];

  config = {
    # allow unfree software on all machines
    nixpkgs.config.allowUnfree = true;
  };
}
