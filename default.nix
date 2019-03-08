# let
#   # nixpkgsVersion = builtins.fromJSON (builtins.readFile ./external/nixpkgs/unstable/version.json);
#   # nixpkgs = builtins.fetchTarball {
#   #   inherit (nixpkgsVersion) url sha256;
#   # };
#   nixpkgs = builtins.fetchTarball {
#     url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
#   };
# in
# { pkgs ? import nixpkgs { config = { }; overlays = [ ]; } }:
{ pkgs ? import <nixpkgs> { config = { }; overlays = [ ]; } }:

with pkgs;

{
  external = import ./external { inherit stdenvNoCC; };
}
