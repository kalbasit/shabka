{ pkgs, ... }:

with pkgs;

let
  homeManager =
    let
      nixpkgs = import ../../../external/nixpkgs-stable.nix;
      pkgs = import nixpkgs {
        config = {};
        overlays = [];
      };
    in import ../../../external/home-manager.nix {
      inherit (pkgs) fetchpatch runCommand;
    };
in {
  home.packages = [
    amazon-ecr-credential-helper
    docker-credential-gcr

    bat

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

    nur.repos.kalbasit.swm

    unstable.corgi
    unstable.vgo2nix

    unzip

    nix-zsh-completions

    unstable.slack-cli
  ] ++ (if stdenv.isLinux then [
    #
    # Linux applications
    #

    jetbrains.idea-community

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
