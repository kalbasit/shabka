{ pkgs ? import <nixpkgs> { config = {}; overlays = []; } }:

with pkgs;

{
  path = ./.;
  external = import ./external { inherit stdenvNoCC; };
}
