self: super:

{
  rbrowser = import ./pkg.nix { inherit (super) stdenv pkgs makeDesktopItem; };
}
