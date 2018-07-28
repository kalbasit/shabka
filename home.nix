{ pkgs, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  programs.home-manager = {
    enable = true;
    # path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
    path = "${system-path}/external/home-manager";
  };

  home.packages = with pkgs; [
      alacritty
      alacritty-config

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

      chromium
      chromium-config

      direnv

      firefox

      fzf

      gist

      git
      git-config

      go

      i3-config
      i3status-config
      dunst dunst-config

      jq

      lastpass-cli

      less-config

      mercurial

      mosh

      most
      most-config

      neovim

      nix-index

      # curses-based file manager
      ranger

      rbrowser

      rofi-config

      surfingkeys-config

      sway-config

      swm

      termite        # Arch-only: this is required to make the ~/.terminfo link happy
      termite-config

      tmux
      tmux-config

      unzip

      zsh
      zsh-config
      nix-zsh-completions
  ];
}
