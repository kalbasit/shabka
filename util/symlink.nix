{ lib }:

let
  pinnedHM = import ../external/home-manager.nix;
in

with import "${pinnedHM}/modules/lib/dag.nix" { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    mkdir -p ${builtins.dirOf dst}
    ln -Tsf ${src} ${dst}
  '';
}
