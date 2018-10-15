{ config, pkgs, ... }:

{
  nixpkgs.config = import ./nixpkgs-config.lib.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.lib.nix;

  nixpkgs.overlays = import ../../../overlays;
}
