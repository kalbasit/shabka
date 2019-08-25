{ lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
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

  nixpkgs.overlays = import <shabka/overlays>;
}
