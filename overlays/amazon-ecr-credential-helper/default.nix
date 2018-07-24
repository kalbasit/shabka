self: super:

{
  # TODO: remove this once https://github.com/NixOS/nixpkgs/pull/44050 is merged
  amazon-ecr-credential-helper = import ./go-package.nix { inherit (super) stdenv buildGoPackage fetchgit fetchhg fetchbzr fetchsvn; };
}
