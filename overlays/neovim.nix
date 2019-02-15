self: super:

{
  neovim = super.unstable.neovim.override { withNodeJs = true; };
  vimPlugins = super.unstable.vimPlugins;
  wrapNeovim = super.unstable.wrapNeovim;
}
