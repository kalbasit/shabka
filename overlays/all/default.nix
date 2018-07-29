self: super:

{
  alacritty-config   = self.callPackage ./alacritty-config {};
  i3-config          = self.callPackage ./i3-config {};
  i3status-config    = self.callPackage ./i3status-config {};
  chromium-config    = self.callPackage ./chromium-config {};
  dunst-config       = self.callPackage ./dunst-config {};
  less-config        = self.callPackage ./less-config {};
  most-config        = self.callPackage ./most-config {};
  surfingkeys-config = self.callPackage ./surfingkeys-config {};
  sway-config        = self.callPackage ./sway-config {};
  termite-config     = self.callPackage ./termite-config {};
  tmux-config        = self.callPackage ./tmux-config {};
  zsh-config         = self.callPackage ./zsh-config {};
}
