{ pkgs, ... }:

{
    home.packages = with pkgs; [
      amazon-ecr-credential-helper
      docker-credential-gcr

      bat

      browsh

      chroot-enter

      corgi

      gist

      gnupg

      go_1_10

      jq

      keybase

      killall

      lastpass-cli

      mercurial

      mosh

      nixify

      nix-index

      nixops

      # curses-based file manager
      lf

      swm

      unzip

      nix-zsh-completions

      # Games
      _2048-in-terminal
    ];
}
