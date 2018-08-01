self: super:

{
  # TODO: remove this overlay once https://github.com/NixOS/nixpkgs/pull/44251 lands in my external
  browsh = super.callPackage ./go-package.nix {};
}
