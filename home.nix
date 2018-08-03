{ pkgs, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  imports = [
    ./modules/home-manager/alacritty
    ./modules/home-manager/chromium
    ./modules/home-manager/dunst
    ./modules/home-manager/git
    ./modules/home-manager/neovim
  ];

  services.gpg-agent = {
    enable = true;

    defaultCacheTtl = 68400;
    maxCacheTtl = 68400;
  };

  programs.rofi = {
    enable = true;

    extraConfig = ''
      rofi.modi: window,run,ssh,drun,i3Workspaces:i3-switch-workspaces,i3RenameWorkspace:i3-rename-workspace,i3MoveContainer:i3-move-container,SwayWorkspaces:sway-switch-workspaces,SwayRenameWorkspace:sway-rename-workspace,SwayMoveContainer:sway-move-container
    '';

    font = "SourceCodePro 9";

    theme = "Adapta-Nokto";
  };

  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  programs.home-manager = {
    enable = true;
    # path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    path = "${system-path}/external/home-manager";
  };

  home.packages = with pkgs; [
    # Applications
    amazon-ecr-credential-helper
    docker-credential-gcr

    bat
    (pkgs.writeTextFile {
      name = "alias-bat-cat";
      destination = "/userHome/.zsh/rc.d/alias-bat-cat.zsh";
      text = ''
          alias cat=bat
      '';
    })

    browsh

    direnv

    firefox

    fzf

    gist

    gnupg

    go

    htop

    i3-config
    i3status-config

    jq

    keybase
    keybase-gui

    lastpass-cli

    less-config

    mercurial

    mosh

    most
    most-config

    nix-index

    nixops

    # curses-based file manager
    ranger

    rbrowser

    surfingkeys-config

    sway-config

    swm

    task-config

    termite        # Arch-only: this is required to make the ~/.terminfo link happy
    termite-config

    tmux
    tmux-config

    unzip

    zsh
    zsh-config
    nix-zsh-completions

    # Games
    _2048-in-terminal
  ];
}
