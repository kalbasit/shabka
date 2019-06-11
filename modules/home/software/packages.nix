{ pkgs, ... }:

with pkgs;

let
  shabka = import <shabka> { };
in {
  home.packages = [
    gnupg

    killall

    mosh

    nix-index

    nixops

    unzip

    nix-zsh-completions
  ] ++ (if stdenv.isLinux then [
    #
    # Linux applications
    #

    keybase
  ] else if stdenv.isDarwin then [
    #
    # Mac-only applications
    #

  ] else []);

  programs.bat.enable = true;
  programs.command-not-found.enable = true;
  programs.direnv.enable = true;

  # install home-manager but only if it's darwin
  programs.home-manager = if stdenv.isDarwin then {
    enable = true;
    path = builtins.toString shabka.external.home-manager.path;
  } else {};
}
