{ pkgs, shabka ? import <shabka> { inherit pkgs; }, lib, ... }:

with lib;

{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = recursiveUpdate
        (import shabka.external.nur.path { inherit pkgs; })
        ({
          repos = {
            kalbasit = import shabka.external.kalbasit.nur.path { inherit pkgs; };
          };
        });
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
