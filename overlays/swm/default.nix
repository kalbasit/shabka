self: super:

{
  swm = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
