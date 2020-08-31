{ lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  nixpkgs.config = { allowUnfree = true; };

  nixpkgs.overlays = import <shabka/overlays>;
}
