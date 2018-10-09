{ pkgs ? import <nixpkgs> {}, ... }:

let

in {
  imports = [
    ../../overlays

    ../modules/dropbox
    ../modules/lowbatt

    ./git
    ./less
    ./neovim
    ./taskwarrior
  ];
}
