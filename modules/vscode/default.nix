{ userSettings ? {}
, keyboardLayout ? "qwerty"
, pkgs
, lib ? pkgs.lib
}:

{
  userSettings = {
    "vim.enableNeovim" = true;
    "vim.hlsearch" = true;
    "vim.incsearch" = true;
    "vim.leader" = ";";
    "vim.neovimPath" = "${lib.getBin pkgs.neovim}/bin/nvim";
    "vim.useCtrlKeys" = true;
    "vim.useSystemClipboard" = true;
    "vim.changeWordIncludesWhitespace" = true;
  }
  // userSettings
  // (import (./keyboard_layouts + "/${keyboardLayout}.nix"));
}
