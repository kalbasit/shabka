self: super:

{
  # TODO: remove this overlay once https://github.com/NixOS/nixpkgs/pull/44033 lands in my external
  pet = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
