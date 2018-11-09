self: super:

{
  vimPlugins = super.unstable.vimPlugins;
  neovim = super.unstable.neovim.override { withNodeJs = true; };
}
