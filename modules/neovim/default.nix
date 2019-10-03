{ extraRC ? ""
, extraKnownPlugins ? {}
, extraPluginDictionaries ? []
, keyboardLayout ? "qwerty"
, pkgs
, viAlias ? true
, vimAlias ? true
, withNodeJs ? true
, withPython ? true
, withPython3 ? true
, withRuby ? true
}:

with pkgs.lib;

{
  inherit viAlias vimAlias withNodeJs withPython withPython3 withRuby;

  extraPythonPackages = ps: with ps; [ pynvim ];
  extraPython3Packages = ps: with ps; [ pynvim ];

  configure = {
    customRC = builtins.concatStringsSep " " [
      (builtins.readFile (pkgs.substituteAll {
        src = ./init.vim;

        ag_bin = "${getBin pkgs.ag}/bin/ag";
        gocode_bin = "${getBin pkgs.nur.repos.kalbasit.gocode}/bin/gocode";
        xsel_bin = "${getBin pkgs.xsel}/bin/xsel";
      }))

      (builtins.readFile (./keyboard_layouts + "/${keyboardLayout}.vim"))

      extraRC
    ];

    vam.knownPlugins = pkgs.vimPlugins // extraKnownPlugins;
    vam.pluginDictionaries = extraPluginDictionaries ++ [
      {
        names =
          [
            "Gist"
            "Gundo"
            "LanguageClient-neovim"
            "PreserveNoEOL"
            "ack-vim"
            "ale"
            "auto-pairs"
            "caw"
            "csv-vim"
            "direnv-vim"
            "easy-align"
            "easymotion"
            "editorconfig-vim"
            "emmet-vim"
            "fzf-vim"
            "fzfWrapper"
            "goyo"
            "multiple-cursors"
            "ncm2"
            "pig-vim"
            "repeat"
            "rhubarb"
            "sleuth"
            "surround"
            "traces-vim"
            "vim-airline"
            "vim-airline-themes"
            "vim-better-whitespace"
            "vim-eunuch"
            "vim-fugitive"
            "vim-markdown"
            "vim-scala"
            "vim-signify"
            "vim-speeddating"
            #"vim-terraform"
            "vimtex"
            "vissort-vim"
            "zoomwintab-vim"

            # NOTE: Keep vim-go before PolyGlot. If PolyGlot is loaded first, vim-go will fail with the error `E117: Unknown function: go#config#VersionWarning`.
            # See https://github.com/sheerun/vim-polyglot/issues/309
            "vim-go"
            "polyglot"

            ## DeoPlete completion support
            "deoplete-nvim"

            # Golang support
            "deoplete-go"

            # TODO(#152): vim-bazel is broken.
            # "vim-maktaba"
            # "vim-bazel"

            # Typescript support
            # "vim-typescript"    # TODO: https://github.com/kalbasit/dotfiles/issues/15
            "yats-vim"
          ]
          ++ (if keyboardLayout == "colemak" then ["vim-colemak"] else []);
      }
    ];
  };
}
