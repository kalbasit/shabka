{
  allowUnfree = true;

  packageOverrides = pkgs_: with pkgs_; {
    nvim-config = import ./nvim-config { inherit pkgs ; };

    git-config = import ./git-config {
      inherit (pkgs) stdenv nvim-config;
    };

    less-config = import ./less-config {
      inherit (pkgs) stdenv less;
    };

    most-config = import ./most-config {
      inherit (pkgs) stdenv;
    };

    surfingkeys-config = import ./surfingkeys-config {
      inherit (pkgs) stdenv;
    };

    termite-config = import ./termite-config {
      inherit (pkgs) stdenv;
    };

    sway-config = import ./sway-config {
      inherit (pkgs) stdenv brightnessctl pulseaudio i3lock rofi termite libnotify slack zsh-config nvim-config i3status;
    };

    swm = import ./swm {
      inherit (pkgs) stdenv buildGoPackage fetchgit;
    };

    tmux-config = import ./tmux-config {
      inherit (pkgs) stdenv tmux xsel gist libnotify;
    };

    zsh-config = import ./zsh-config {
      inherit (pkgs) stdenv;
    };

    all = with pkgs; buildEnv {
      name = "all";

      paths = [
        fzf

        git
        git-crypt
        git-config

        go
        dep
        swm

        less-config

        mosh

        most
        most-config

        nvim-config

        nix-index

        nodejs-8_x

        powerline-fonts

        python27
        python36

        surfingkeys-config

        sway-config

        termite
        termite-config

        tmux
        tmux-config

        zsh
        zsh-config
        nix-zsh-completions
      ];
    };
  };
}

# TODO: remove all comments below once it's settled

# firefox = {
#   enableGoogleTalkPlugin = true;
#   enableAdobeFlash = true;
# };
#
# chromium = {
#   enablePepperFlash = true;
# };

# gtk-config = import ./gtk-config {
#   inherit (pkgs) stdenv albatross;
# };
# termite-config = import ./termite-config {
#   inherit (pkgs) stdenv;
#   vte = gnome3.vte;
# };
# qtile-config = import ./qtile-config {
#   inherit (pkgs) stdenv;
# };
# bash-config = import ./bash-config {
#   inherit (pkgs) stdenv fzf ; jdk = oraclejdk;
# };
# elixir-config = import ./elixir-config {
#   inherit (pkgs) stdenv;
# };

# core
# bash
# file

# nix helpers
# nix-prefetch-scripts
# nix-repl
# nixops
# nixpkgs-lint
# nox
# patchelf
# patchutils

# utilities
# gist
# gnupg
# htop
# httpie
# inotify-tools
# jq
# libnotify
# libu2f-host # support for Yubikey on Chromium
# lsof
# tmux
# tree
# unzip
# virtualbox    # TODO: enable VirtualBox on NixOS


# development tools
# gnumake
# go dep
# jdk

# networking tools
# curl
# mosh
# ncat
# wget
#
# sway

# i3 and helpers
#i3blocks
#i3status
#rofi
#termite
#xcape
#xorg.xbacklight
#xorg.xdpyinfo
#xorg.xmodmap
#xsel
#xss-lock

# Applications
# chromium
# firefox


# paths = [
#   gtk-config
#   termite-config
#   qtile-config
#   bash-config
#   elixir-config

#   nix-repl
#   nix-prefetch-scripts
#   nixpkgs-lint
#   nixops
#   nox
#   patchelf
#   patchutils

#   telnet
#   wireshark-gtk
#   bind
#   netcat-openbsd

#   termite
#   cv
#   powerline-fonts
#   clipit
#   xsel
#   ntfy

#   pasystray
#   pavucontrol
#   alsaUtils

#   blueman

#   arandr

#   gnumake

#   tree
#   inotify-tools
#   fzf
#   ripgrep
#   fd
#   ranger
#   my_vim
#   atom
#   typora

#   git
#   git-radar
#   gitg
#   meld
#   tig

#   bazaar
#   mercurial

#   zip
#   unzip
#   p7zip

#   bc

#   firefoxWrapper
#   chromium
#   httpie
#   mitmproxy

#   tokei
#   jq

#   go

#   beam.packages.erlangR20.elixir

#   nodejs-8_x

#   oraclejdk
#   maven
#   #idea.idea-ultimate
#   #heroku

#   python36
#   gcc

#   nim
#   ponyc

#   python36Packages.glances

#   python36Packages.docker_compose
#   gparted
#   proot
#   vagrant
#   ansible2

#   evince
#   libreoffice
#   vlc
#   ffmpeg
#   geeqie
#   pinta
#   inkscape
#   graphicsmagick
#   yed
#   zoom-us

#   deluge

#   hexchat
# ];
