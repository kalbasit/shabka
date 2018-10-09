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
    ./zsh
  ];


  # enable htop
  programs.htop = {
    enable = true;
  };

  # enable flameshot screenshot daemon. Use `flameshot gui` to start taking screenshot.
  services.flameshot.enable = true;

  # Enable direnv
  programs.direnv.enable = true;


}
