{ pkgs, lib, ... }:

{
  programs.vscode = {
    enable = true;
    userSettings = {
      "vim.enableNeovim" = true;
      "vim.hlsearch" = true;
      "vim.incsearch" = true;
      "vim.leader" = ";";
      "vim.neovimPath" = "${lib.getBin pkgs.neovim}/bin/nvim";
      "vim.useCtrlKeys" = true;
      "vim.useSystemClipboard" = true;
      "vim.changeWordIncludesWhitespace" = true;
      "vim.visualModeKeyBindingsNonRecursive" = [
        {
          before = ["n"];
          after = ["<left>"];
        }
        {
          before = ["e"];
          after = ["<down>"];
        }
        {
          before = ["i"];
          after = ["<up>"];
        }
        {
          before = ["o"];
          after = ["<right>"];
        }
        {
          before = ["c"];
          after = ["y"];
        }
        {
          before = ["v"];
          after = ["p"];
        }
      ];

      "vim.normalModeKeyBindingsNonRecursive" = [
        {
          before = ["n"];
          after = ["<left>"];
        }
        {
          before = ["e"];
          after = ["<down>"];
        }
        {
          before = ["i"];
          after = ["<up>"];
        }
        {
          before = ["o"];
          after = ["<right>"];
        }
        {
          before = ["s"];
          after = ["i" ];
        }
        {
          before = ["<leader>" "w" "w"];
          after = [];
          commands = [
            {
              command = ":wall";
            }
          ];
        }
        {
          before = ["h"];
          after = ["o"];
        }
        {
          before = ["H"];
          after = ["O"];
        }
        {
          before = ["<c-j>"];
          after = ["<c-n>"];
        }
        {
          before = ["k"];
          after = ["n"];
        }
        {
          before = ["K"];
          after = ["N"];
        }
        {
          before = ["u"];
          after = ["i"];
        }
        {
          before = ["U"];
          after = ["I"];
        }
        {
          before = ["l"];
          after = ["u"];
        }
        {
          before = ["L"];
          after = ["U"];
        }
        {
          before = ["N"];
          after = ["J"];
        }
        {
          before = ["E"];
          after = ["K"];
        }
        {
          before = ["I"];
          after = ["L"];
        }
        {
          before = ["j"];
          after = ["e"];
        }
        {
          before = ["J"];
          after = ["E"];
        }
        {
          before = ["t"];
          after = ["a"];
        }
        {
          before = ["T"];
          after = ["A"];
        }
        {
          before = ["c"];
          after = ["y"];
        }
        {
          before = ["C"];
          after = ["Y"];
        }
        {
          before = ["v"];
          after = ["p"];
        }
        {
          before = ["V"];
          after = ["P"];
        }
        {
          before = ["a"];
          after = ["v"];
        }
        {
          before = ["A"];
          after = ["V"];
        }
      ];
    };
  };
}
