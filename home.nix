{ pkgs, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
in {
  imports = [
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

    font = "Cousine 9";

    theme = "Adapta-Nokto";
  };

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

      gnupg

      go

      htop

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

      nix-index

      # curses-based file manager
      ranger

      rbrowser

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
