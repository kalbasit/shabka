{ lib }:

with import <home-manager/modules/lib/dag.nix> { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    mkdir -p ${builtins.dirOf dst}
    ln -Tsf ${src} ${dst}
  '';
}
