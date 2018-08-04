self: super:

{
  sway-config        = self.callPackage ./sway-config {};
  zsh-config         = self.callPackage ./zsh-config {};
}
