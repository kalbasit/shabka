{ lib }:

let
  shabka = import <shabka> { };
in

with import "${shabka.external.home-manager.release-unstable.path}/modules/lib/dag.nix" { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    mkdir -p ${builtins.dirOf dst}
    ln -Tsf ${src} ${dst}
  '';
}
