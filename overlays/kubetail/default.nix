self: super:

{
  # TODO: remove this once https://github.com/NixOS/nixpkgs/pull/44062 is merged
  kubetail = super.callPackage ./pkg.nix {};
}
