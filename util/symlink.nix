{ lib }:

with import <home-manager/modules/lib/dag.nix> { inherit lib; };

{
  symlink = src: dst: dagEntryAfter ["installPackages"] ''
    if [ ! -L ${dst} ]; then
    ln -s ${src} ${dst}
    fi
  '';
}
