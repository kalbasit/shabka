{ pkgs ? import <nixpkgs> { config = { }; overlays = [ ]; } }:

with pkgs;

{
  external = callPackage ./external { };
}
