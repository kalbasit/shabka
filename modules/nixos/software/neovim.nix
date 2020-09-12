{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.shabka.neovim;

  keyboardLayout = builtins.head config.shabka.keyboard.layouts;

  neovimConfig = import ../../neovim {
    inherit (cfg) extraRC extraKnownPlugins extraPluginDictionaries;
    inherit pkgs;
    inherit keyboardLayout;
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
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (wrapNeovim neovim.unwrapped neovimConfig)
    ];
  };
}
