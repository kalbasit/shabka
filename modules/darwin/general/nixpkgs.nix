{ pkgs, lib, ... }:

with lib;

{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = recursiveUpdate
        (import pkgs.shabka.external.nur.path { inherit pkgs; })
        ({
          repos = {
            kalbasit = import pkgs.shabka.external.kalbasit.nur.path { inherit pkgs; };
          };
        });
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
