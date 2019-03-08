{ pkgs ? import <nixpkgs> { config = { }; overlays = [ ]; } }:

with pkgs;

{
  home-manager = callPackage ./home-manager { };
  kalbasit = callPackage ./kalbasit { };
  nix-darwin = callPackage ./nix-darwin { };
  nixos-hardware = callPackage ./nixos-hardware { };
  nixpkgs = callPackage ./nixpkgs { };
  nur = callPackage ./nur { };
}
