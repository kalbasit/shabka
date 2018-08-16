{ pkgs ? import <nixpkgs> {}, ... }:

let

in {
  imports = [
    ../modules/dropbox
    ../modules/lowbatt

    ./alacritty
    ./chromium
    ./dunst
    ./firefox
    ./git
    ./i3
    ./neovim
    ./rofi
    ./taskwarrior
    ./termite
    ./zsh

    # TODO: enable this once https://github.com/erebe/greenclip/issues/39 has
    # been resolved, and released to HackagePackages.
    # ./cfg/home-manager/greenclip
  ];

  # setup nixpkgs config
  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  # set the keyboard layout and variant
  home.keyboard.layout = "us";
  home.keyboard.variant = "colemak";

  services.gpg-agent = {
    enable = true;

    defaultCacheTtl = 68400;
    maxCacheTtl = 68400;
  };

  # enable the screen locker
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock-color}/bin/i3lock-color --clock --color=606060";
    inactiveInterval = 15;
  };

  # enable Dropbox
  services.dropbox.enable = true;

  # enable batteryNotifier
  services.batteryNotifier.enable = true;

  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # enable FZF
  programs.fzf = {
    enable = true;
  };

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

    gist

    gnupg

    go

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
    ranger

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
