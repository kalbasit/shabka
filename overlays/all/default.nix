self: super:

{
  alacritty-config = self.callPackage ./alacritty-config {};

  i3-config = self.callPackage ./i3-config {};

  i3status-config = self.callPackage ./i3status-config {};

  git-config = self.callPackage ./git-config {};

  less-config = self.callPackage ./less-config {};

  most-config = self.callPackage ./most-config {};

  nvim-config = self.callPackage ./nvim-config {};

  rbrowser = self.callPackage ./rbrowser {};

  rofi-config = self.callPackage ./rofi-config {};

  surfingkeys-config = self.callPackage ./surfingkeys-config {};

  sway-config = self.callPackage ./sway-config {};

  swm = self.callPackage ./swm {};

  tmux-config = self.callPackage ./tmux-config {};

  zsh-config = self.callPackage ./zsh-config {};

  all = with self; buildEnv {
    name = "all";

    paths = [
      alacritty
      alacritty-config

      bat

      chromium

      direnv

      firefox

      fzf

      git
      git-crypt
      git-config

      go
      dep
      swm

      i3-config
      i3status-config

      jq

      less-config

      mercurial

      mosh

      most
      most-config

      nvim-config
      gocode
      nodejs

      nix-index

      powerline-fonts

      rbrowser

      rofi-config

      surfingkeys-config

      sway-config

      tmux
      tmux-config

      zsh
      zsh-config
      nix-zsh-completions
    ];
  };
}
