self: super:

{
  amazon-ecr-credential-helper = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
