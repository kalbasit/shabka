self: super:

{
  download-archiver = super.callPackage ../pkgs/download-archiver {};
  gpg-clean-up = super.callPackage ../pkgs/gpg-clean-up {};
  nix-verify = super.callPackage ../pkgs/nix-verify {};
  rbrowser = super.callPackage ../pkgs/rbrowser {};
  swm = super.callPackage ../pkgs/swm {};
}
