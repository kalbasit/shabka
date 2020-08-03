{ pkgs, ... }:

with pkgs;

let
  shabka = import <shabka> { };
  release = builtins.getEnv "RELEASE";
in {
  home.packages = [
    amazon-ecr-credential-helper
    docker-credential-gcr

    browsh

    gist

    gnupg

    go

    gotop

    jq

    jrnl

    killall

    lastpass-cli

    mercurial

    mosh

    nur.repos.kalbasit.nixify

    nix-index

    nixops

    # curses-based file manager
    lf

    swm

    shabka.external.nixpkgs.release-unstable.corgi
    shabka.external.nixpkgs.release-unstable.vgo2nix

    unzip

    nix-zsh-completions
  ] ++ (if stdenv.isLinux then [
    #
    # Linux applications
    #

    jetbrains.idea-community

    keybase

    slack

    # Games
    _2048-in-terminal
  ] else if stdenv.isDarwin then [
    #
    # Mac-only applications
    #

  ] else []);

  programs.bat.enable = true;
  programs.direnv.enable = true;

  # install home-manager but only if it's darwin
  programs.home-manager = if stdenv.isDarwin then {
    enable = true;
    path = builtins.toString shabka.external.home-manager."${release}".path;
  } else {};
}
