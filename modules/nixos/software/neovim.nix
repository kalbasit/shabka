{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.neovim;

  keyboardLayout = builtins.head config.mine.keyboard.layouts;

  neovimConfig = import ../../neovim {
    inherit (cfg) extraRC extraKnownPlugins extraPluginDictionaries;
    inherit pkgs;
    inherit keyboardLayout;
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
  };

  config = mkIf cfg.enable {
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
