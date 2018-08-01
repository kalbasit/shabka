self: super:

{
  # TODO: remove this overlay once https://github.com/NixOS/nixpkgs/pull/44062 lands in my external
  kubetail = super.callPackage ./pkg.nix {};
}
