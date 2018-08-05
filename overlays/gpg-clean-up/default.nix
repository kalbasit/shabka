self: super:

{
  gpg-clean-up = super.callPackage ./pkg.nix {};
}
