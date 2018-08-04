self: super:

{
  greenclip-config   = self.callPackage ./greenclip-config {};
  less-config        = self.callPackage ./less-config {};
  most-config        = self.callPackage ./most-config {};
  surfingkeys-config = self.callPackage ./surfingkeys-config {};
  sway-config        = self.callPackage ./sway-config {};
  task-config        = self.callPackage ./task-config {};
  termite-config     = self.callPackage ./termite-config {};
  tmux-config        = self.callPackage ./tmux-config {};
  zsh-config         = self.callPackage ./zsh-config {};
}
