{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.neovim;

  neovimConfig = import ../../neovim {
    inherit (cfg) extraRC extraKnownPlugins extraPluginDictionaries keyboardLayout;
    inherit pkgs;
  };

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

    keyboardLayout = mkOption {
      type = with types; enum [ "colemak" "qwerty" ];
      default = if config.mine.useColemakKeyboardLayout then "colemak" else "qwerty";
      description = ''
        The keyboard layout to use.
      '';
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      direnv

      (wrapNeovim neovim.unwrapped {
        inherit (neovimConfig)
        extraPython3Packages withPython3
        extraPythonPackages withPython
        withNodeJs withRuby viAlias vimAlias configure;
      })
    ];
  };
}
