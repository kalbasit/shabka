self: super:

{
  neovim = super.external.nixpkgs.unstable.neovim.override { withNodeJs = true; };
  vimPlugins = super.external.nixpkgs.unstable.vimPlugins;
  wrapNeovim = super.external.nixpkgs.unstable.wrapNeovim;
}
