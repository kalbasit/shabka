self: super:

{
  # TODO: remove this overlay once https://github.com/NixOS/nixpkgs/pull/44050 lands in my external
  amazon-ecr-credential-helper = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
