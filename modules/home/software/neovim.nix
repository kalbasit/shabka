{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.shabka.neovim;

  neovimConfig = import ../../neovim {
    inherit (cfg) extraRC extraKnownPlugins extraPluginDictionaries keyboardLayout;
    inherit pkgs;
  };

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
      default = (builtins.head config.shabka.keyboard.layouts);
      description = ''
        The keyboard layout to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.neovim = neovimConfig // {
      inherit (cfg) enable;
    };
  };
}
