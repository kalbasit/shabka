self: super:

{
  sway-config        = self.callPackage ./sway-config {};
  tmux-config        = self.callPackage ./tmux-config {};
  zsh-config         = self.callPackage ./zsh-config {};
}
