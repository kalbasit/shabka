self: super:

{
  # TODO: remove the following override once the below PR is merged.
  # - https://github.com/NixOS/nixpkgs/pull/43995
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs(oa: {
    postInstall = ''
      sed -i -e "s|'xsel|'${super.xsel}/bin/xsel|g" $out/share/nvim/runtime/autoload/provider/clipboard.vim
    '';
  });
}
