{ pkgs, lib, }:

let
  home-manager-version = lib.importJSON ./home-manager-version.json;
  pinned-home-manager = pkgs.fetchFromGitHub {
    owner = "rycee";
    repo = "home-manager";
    inherit (home-manager-version) rev sha256;
  };
in
  pinned-home-manager
