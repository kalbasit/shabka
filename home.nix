{ pkgs, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  imports = [
    ./modules/home-manager/alacritty
    ./modules/home-manager/chromium
    ./modules/home-manager/dunst
    ./modules/home-manager/git
    ./modules/home-manager/i3
    ./modules/home-manager/neovim
    ./modules/home-manager/rofi
    ./modules/home-manager/taskwarrior
    ./modules/home-manager/termite
    ./modules/home-manager/zsh

    # TODO: enable this once https://github.com/erebe/greenclip/issues/39 has
    # been resolved, and released to HackagePackages.
    # ./modules/home-manager/greenclip
  ];

  # set the keyboard layout and variant
  home.keyboard.layout = "us";
  home.keyboard.variant = "colemak";

  services.gpg-agent = {
    enable = true;

    defaultCacheTtl = 68400;
    maxCacheTtl = 68400;
  };

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
