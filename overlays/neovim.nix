self: super:

{
  neovim = super.shabka.external.nixpkgs.release-unstable.neovim.override { withNodeJs = true; };
  vimPlugins = super.shabka.external.nixpkgs.release-unstable.vimPlugins;
  wrapNeovim = super.shabka.external.nixpkgs.release-unstable.wrapNeovim;
}
