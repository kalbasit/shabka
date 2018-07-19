self: super:

let
  # Look here for information about how to generate `nixpkgs-version.json`.
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedVersion = super.lib.importJSON ./nixpkgs-version.json;
  pinnedPkgs = super.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    inherit (pinnedVersion) rev sha256;
  };

  nixpkgs = import pinnedPkgs {};
  config = {};
in

{
  haskell.packages.greenclip = pinnedPkgs.haskell.packages.greenclip;
}
