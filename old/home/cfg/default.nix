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


  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # enable htop
  programs.htop = {
    enable = true;
  };

  # enable flameshot screenshot daemon. Use `flameshot gui` to start taking screenshot.
  services.flameshot.enable = true;

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

    chroot-enter

    corgi

    gist

    gnupg

    go_1_10

    jq

    keybase
    keybase-gui

    killall

    lastpass-cli

    mercurial

    mosh

    nixify

    nix-index

    nixops

    pet

    # curses-based file manager
    lf

    # NOTE: Slack does not seem to find the rbrowser.desktop in
    #       ~/.nix-profile/share/applications so you must manually create a
    #       symlink to ~/.local/share/applications on bootstrap.
    # ln -s ../../../.nix-profile/share/applications/rbrowser.desktop ~/.local/share/applications/rbrowser.desktop
    rbrowser

    remmina

    swm

    unzip

    nix-zsh-completions

    weechat

    xsel

    # zoom for meetings
    zoom-us

    # Games
    _2048-in-terminal
  ];

  # configure pet
  programs.zsh.initExtra = ''
    # setup pet
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
