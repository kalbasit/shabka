{ pkgs ? import <nixpkgs> { config = {}; overlays = []; } }:

with pkgs;

{
  external = import ./external { inherit stdenvNoCC; };
}
