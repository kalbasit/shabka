{ lib }:

let

  homeManager = import ../external/home-manager.nix;

in

with import "${homeManager}/modules/lib/dag.nix" { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    mkdir -p ${builtins.dirOf dst}
    ln -Tsf ${src} ${dst}
  '';
}
