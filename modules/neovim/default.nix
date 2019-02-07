{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.shabka.neovim;

in {
  options.shabka.neovim = {
    enable = mkEnableOption "neovim";

    extraRC = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra NeoVim init configuration.
      '';
    };

    extraKnownPlugins = mkOption {
      type = types.attrs;
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

    keyboardLayout = mkOption {
      type = types.enum [ "colemak" "qwerty" ];
      default = "qwerty";
      description = ''
        The keyboard layout to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    viAlias = true;
    vimAlias = true;

    withPython = true;
    extraPythonPackages = ps: with ps; [ pynvim ];

    withPython3 = true;
    extraPython3Packages = ps: with ps; [ pynvim ];

    configure = {
      customRC = builtins.concatStringsSep " " [
        (builtins.readFile (pkgs.substituteAll {
          src = ./init.vim;

          ag_bin = "${pkgs.ag}/bin/ag";
          gocode_bin = "${pkgs.nur.repos.kalbasit.gocode}/bin/gocode";
          xsel_bin = "${pkgs.xsel}/bin/xsel";
        }))

        cfg.extraRC

        (builtins.readFile ./keyboard_layouts + "${cfg.keyboardLayout}.vim")
      ];

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
              "vim-signify"
              "vim-speeddating"
              "vim-terraform"
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

              "vim-maktaba"
              "vim-bazel"


              # Typescript support
              # "vim-typescript"    # TODO: https://github.com/kalbasit/dotfiles/issues/15
              "yats-vim"

            ]
            ++ (if cfg.keyboardLayout == "colemak" then ["vim-colemak"] else []);
        }
      ];
    };
  };
}
