self: super:

# TODO: This should be a per-host setting if shabka is for everyone to use!

{
  neovim = super.shabka.external.nixpkgs.release-unstable.neovim.override { withNodeJs = true; };
  neovim-unwrapped = super.shabka.external.nixpkgs.release-unstable.neovim-unwrapped;
  vimPlugins = super.shabka.external.nixpkgs.release-unstable.vimPlugins;
  wrapNeovim = super.shabka.external.nixpkgs.release-unstable.wrapNeovim;
}
