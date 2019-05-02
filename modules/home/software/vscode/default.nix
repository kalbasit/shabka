{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.vscode;

  vscodeConfig = import ../../../vscode {
    inherit (cfg) userSettings keyboardLayout;
    inherit pkgs;
  };

in {
  options.mine.vscode = {
    enable = mkEnableOption "vscode";

    userSettings = mkOption {
      type = types.attrs;
      default = {};
      example = literalExample ''
        {
          "update.channel" = "none";
          "[nix]"."editor.tabSize" = 2;
        }
      '';
      description = ''
        Configuration written to
        <filename>~/.config/Code/User/settings.json</filename>.
      '';
    };

    extensions = mkOption {
      type = types.listOf types.package;
      default = [];
      description = ''
        The extensions Visual Studio Code should be started with.
        These will override but not delete manually installed ones.
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

  config = mkIf cfg.enable {
    programs.vscode = vscodeConfig // {
      inherit (cfg) enable extensions;
    };
  };
}
