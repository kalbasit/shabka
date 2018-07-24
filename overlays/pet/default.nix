self: super:

{
  # TODO: remove this once https://github.com/NixOS/nixpkgs/pull/44033 is merged
  pet = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
