{ pkgs, lib, ... }:

with lib;

let
  external = import ../../../external {};
in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = recursiveUpdate
        (import external.nur.path { inherit pkgs; })
        ({
          repos = {
            kalbasit = import external.kalbasit.nur.path { inherit pkgs; };
          };
        });
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
