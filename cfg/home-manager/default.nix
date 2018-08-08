{ pkgs ? import <nixpkgs> {}, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  imports = [
    ../../modules/home-manager/dropbox
    ../../modules/home-manager/lowbatt

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

  nixpkgs.config = {
    allowUnfree = true;

    chromium = {
      enablePepperFlash = true;
    };
  };

  nixpkgs.overlays = [
    (import ../../overlays/download-archiver)
    (import ../../overlays/gpg-clean-up)
    (import ../../overlays/haskellPackages)
    (import ../../overlays/neovim)
    (import ../../overlays/nix-verify)
    (import ../../overlays/nodePackages)
    (import ../../overlays/rbrowser)
    (import ../../overlays/swm)
    (import ../../overlays/tmuxPlugins)
  ];

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

  programs.home-manager = {
    enable = true;
    # path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    path = "${system-path}/external/home-manager";
  };

  # enable FZF
  programs.fzf = {
    enable = true;
  };

  # enable htop
  programs.htop = {
    enable = true;
  };

  # enable compton
  services.compton = {
    enable = true;
  };

  # enable flameshot screenshot daemon. Use `flameshot gui` to start taking screenshot.
  services.flameshot.enable = true;

  # Enable direnv
  programs.direnv.enable = true;

  # Enable the network applet
  services.network-manager-applet.enable = true;

  home.file.".config/nixpkgs/config.nix".text = ''
    {
      allowUnfree = true;

      chromium = {
        enablePepperFlash = true;
      };
    }
  '';

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

    lastpass-cli

    mercurial

    mosh

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

    swm

    unzip

    nix-zsh-completions

    weechat

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

