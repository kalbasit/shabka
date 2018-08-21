{ pkgs, lib, }:

let
  nixpkgs-version = lib.importJSON ./nixpkgs-version.json;
  pinned-nixpkgs = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs-version) rev sha256;
  };
in
  pinned-nixpkgs
