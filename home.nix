{ pkgs ? import <nixpkgs> {}, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  imports = [
    ./modules/home-manager/dropbox
    ./modules/home-manager/lowbatt

    ./cfg/home-manager/alacritty
    ./cfg/home-manager/chromium
    ./cfg/home-manager/dunst
    ./cfg/home-manager/git
    ./cfg/home-manager/i3
    ./cfg/home-manager/neovim
    ./cfg/home-manager/rofi
    ./cfg/home-manager/taskwarrior
    ./cfg/home-manager/termite
    ./cfg/home-manager/zsh

    # TODO: enable this once https://github.com/erebe/greenclip/issues/39 has
    # been resolved, and released to HackagePackages.
    # ./cfg/home-manager/greenclip
  ];

  # set the keyboard layout and variant
  home.keyboard.layout = "us";
  home.keyboard.variant = "colemak";

  services.gpg-agent = {
    enable = true;

    defaultCacheTtl = 68400;
    maxCacheTtl = 68400;
  };

  # enable Dropbox
  services.dropbox.enable = true;

  # enable batteryNotifier
  services.batteryNotifier.enable = true;

  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  programs.home-manager = {
    enable = true;
    # path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    path = "${system-path}/external/home-manager";
  };

  # Enable direnv
  programs.direnv.enable = true;

  # Enable the network applet
  services.network-manager-applet.enable = true;

  home.packages = with pkgs; [
    # Applications
    amazon-ecr-credential-helper
    docker-credential-gcr

    bat

    browsh

    firefox

    fzf

    gist

    gnupg

    go

    htop

    jq

    keybase
    keybase-gui

    lastpass-cli

    mercurial

    mosh

    nix-index

    nixops

    pet

    # curses-based file manager
    ranger

    rbrowser

    swm

    unzip

    nix-zsh-completions

    # Games
    _2048-in-terminal
  ];

  # configure pet
  programs.zsh.initExtra = ''
    function pet_select() {
      BUFFER=$(${pkgs.pet}/bin/pet search --query "$LBUFFER")
      CURSOR=$#BUFFER
      zle redisplay
    }

    function pet_prev() {
      PREV=$(fc -lrn | head -n 1)
      sh -c "${pkgs.pet}/bin/pet new $(printf %q "$PREV")"
    }

    if [[ -o interactive ]]; then
      zle -N pet_select
      stty -ixon
      bindkey '^p' pet_select
    fi
  '';
}
