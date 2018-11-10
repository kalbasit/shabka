{ pkgs, ... }:

with pkgs;

{
  home.packages = [
    amazon-ecr-credential-helper
    docker-credential-gcr

    bat

    browsh

    corgi

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

    nur.repos.kalbasit.swm

    unzip

    nix-zsh-completions
  ] ++ (if stdenv.isLinux then [
    #
    # Linux applications
    #

    keybase

    # Games
    _2048-in-terminal
  ] else if stdenv.isDarwin then [
    #
    # Mac-only applications
    #

  ] else []);

  programs.direnv.enable = true;
  programs.htop.enable = true;

  # install home-manager but only if it's darwin
  programs.home-manager = if stdenv.isDarwin then {
    enable = true;
  } else {};
}
