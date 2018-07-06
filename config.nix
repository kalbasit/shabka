{
  allowUnfree = true;

  packageOverrides = pkgs_: with pkgs_; {
    my_nvim = import ./nvim-config { inherit pkgs ; };

    git-config = import ./git-config {
      # TODO: how to send nvim instead of my_nvim so the packages are not
      # tightly coupled.
      inherit (pkgs) stdenv my_nvim;
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

        my_nvim

        nodejs-8_x

        python27
        python36

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
# nix-index
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
