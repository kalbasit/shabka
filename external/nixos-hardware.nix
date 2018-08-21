{ pkgs ? import <nixpkgs> {} }:

let
  nixos-hardware-version = pkgs.lib.importJSON ./nixos-hardware-version.json;
  pinned-nixos-hardware = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixos-hardware";
    inherit (nixos-hardware-version) rev sha256;
  };
in
  pinned-nixos-hardware
