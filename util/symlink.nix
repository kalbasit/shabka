{ lib }:

let
  homeManager =
    let
      nixpkgs = import ../external/nixpkgs-stable.nix;
      pkgs = import nixpkgs {
        config = {};
        overlays = [];
      };
    in import ../external/home-manager.nix {
      inherit (pkgs) fetchpatch runCommand;
    };
in

with import "${homeManager}/modules/lib/dag.nix" { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    mkdir -p ${builtins.dirOf dst}
    ln -Tsf ${src} ${dst}
  '';
}
