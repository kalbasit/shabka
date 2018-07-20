self: super:

{
  alacritty-config   = self.callPackage ./alacritty-config {};
  git-config         = self.callPackage ./git-config {};
  i3-config          = self.callPackage ./i3-config {};
  i3status-config    = self.callPackage ./i3status-config {};
  chromium-config    = self.callPackage ./chromium-config {};
  dunst-config       = self.callPackage ./dunst-config {};
  less-config        = self.callPackage ./less-config {};
  most-config        = self.callPackage ./most-config {};
  rbrowser           = self.callPackage ./rbrowser {};
  rofi-config        = self.callPackage ./rofi-config {};
  surfingkeys-config = self.callPackage ./surfingkeys-config {};
  sway-config        = self.callPackage ./sway-config {};
  termite-config     = self.callPackage ./termite-config {};
  tmux-config        = self.callPackage ./tmux-config {};
  zsh-config         = self.callPackage ./zsh-config {};

  all = with self; buildEnv {
    name = "all";

    paths = [
      alacritty
      alacritty-config

      bat
      (self.writeTextFile {
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

      less-config

      mercurial

      mosh

      most
      most-config

      neovim

      nix-index

      pet

      powerline-fonts

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
  };
}
