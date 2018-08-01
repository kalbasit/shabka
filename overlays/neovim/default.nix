self: super:

{
  # TODO: remove this overlay once https://github.com/NixOS/nixpkgs/pull/43995 lands in my external
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs(oa: {
    postInstall = ''
      sed -i -e "s|'xsel|'${super.xsel}/bin/xsel|g" $out/share/nvim/runtime/autoload/provider/clipboard.vim
    '';
  });
}
