self: super:

{
  neovim = super.shabka.external.nixpkgs.release-unstable.imported.neovim.override { withNodeJs = true; };
  vimPlugins = super.shabka.external.nixpkgs.release-unstable.imported.vimPlugins;
  wrapNeovim = super.shabka.external.nixpkgs.release-unstable.imported.wrapNeovim;
}
