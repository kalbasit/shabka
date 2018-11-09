{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.neovim;

in {
  options.mine.neovim = {
    enable = mkEnableOption "neovim";

    extraRC = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra NeoVim init configuration.
      '';
    };

    extraKnownPlugins = mkOption {
      default = {};
      description = ''
        Extra NeoVim known plugins.
      '';
    };

    extraPluginDictionaries = mkOption {
      type = with types; listOf attrs;
      default = [];
      description = ''
        Extra NeoVim plugin dictionary.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.neovim = {
      enable = true;

      viAlias = true;
      vimAlias = true;

      withPython = true;
      extraPythonPackages = ps: with ps; [neovim];

      withPython3 = true;
      extraPython3Packages = ps: with ps; [neovim];

      configure = {
        customRC = cfg.extraRC + (builtins.readFile (pkgs.substituteAll {
          src = ./init.vim;

          ag_bin = "${pkgs.ag}/bin/ag";
          gocode_bin = "${pkgs.nur.repos.kalbasit.gocode}/bin/gocode";
          xsel_bin = "${pkgs.xsel}/bin/xsel";
        })) + (if config.mine.useColemakKeyboardLayout then (builtins.readFile ./colemak.vim) else "");

        vam.knownPlugins = pkgs.vimPlugins // cfg.extraKnownPlugins;
        vam.pluginDictionaries = cfg.extraPluginDictionaries ++ [
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
                "nvim-completion-manager"
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
                "vim-signify"
                "vim-speeddating"
                "vim-terraform"
                "zoomwintab-vim"
                "vimtex"
                "vissort-vim"

                # NOTE: Keep vim-go before PolyGlot. If PolyGlot is loaded first, vim-go will fail with the error `E117: Unknown function: go#config#VersionWarning`.
                # See https://github.com/sheerun/vim-polyglot/issues/309
                "vim-go"
                "polyglot"

                ## DeoPlete completion support
                "deoplete-nvim"

                # Golang support
                "deoplete-go"

                "vim-maktaba"
                "vim-bazel"


                # Typescript support
                # "vim-typescript"    # TODO: https://github.com/kalbasit/dotfiles/issues/15
                "yats-vim"

              ]
              ++ (if config.mine.useColemakKeyboardLayout then ["vim-colemak"] else []);
          }
        ];
      };
    };
  };
}
