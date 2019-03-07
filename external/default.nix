{ pkgs ? import <nixpkgs> { config = { }; overlays = [ ]; } }:

with pkgs;

{
  home-manager = callPackage ./home-manager { };
  kalbasit = {
    keys = callPackage ./kalbasit/keys { };
    nur = callPackage ./kalbasit/nur { };
  };
  nix-darwin = callPackage ./nix-darwin { };
  nixos-hardware = callPackage ./nixos-hardware { };
  nixpkgs = {
    release-18-09 = callPackage ./nixpkgs/18.09 { };
    unstable = callPackage ./nixpkgs/unstable { };
  };
}
