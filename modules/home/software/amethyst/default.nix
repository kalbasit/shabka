{ lib, config, ... }:

with lib;

let
  cfg = config.shabka.amethyst;

  keyboardShortcutOptions = {
    options = with types; {
      mod = mkOption {
        type = enum ["mod1" "mod2"];
      };
      key = mkOption {
        type = str;
      };
    };
  };

in {
  imports = [ ./colemak.nix ];

  options.shabka.amethyst = {
    enable = mkEnableOption "Automatic tiling window manager for macOS à la xmonad";

    layouts = mkOption {
      type = with types; listOf str;
      default = [
         "tall"
         "wide"
         "fullscreen"
         "column"
      ];
    };

    mod1 = mkOption {
      type = with types; listOf str;
      default = [
         "option"
         "shift"
      ];
    };

    mod2 = mkOption {
      type = with types; listOf str;
      default = [
        "option"
        "shift"
        "control"
      ];
    };

    screens = mkOption {
      type = types.ints.unsigned;
      default = 4;
    };

    keyboardLayout = mkOption {
      type = with types; enum [ "colemak" "qwerty" ];
      default = (builtins.head config.shabka.keyboard.layouts);
      description = ''
        The keyboard layout to use.
      '';
    };

    cycle-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "space";
      };
    };

    cycle-layout-backward = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "space";
      };
    };

    select-tall-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "a";
      };
    };

    select-wide-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "s";
      };
    };

    select-fullscreen-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "d";
      };
    };

    select-column-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "f";
      };
    };

    focus-screen-ccw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "p";
      };
    };

    focus-screen-cw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "n";
      };
    };

    focus-screen-1 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "w";
      };
    };

    focus-screen-2 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "e";
      };
    };

    focus-screen-3 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "r";
      };
    };

    focus-screen-4 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "q";
      };
    };

    throw-screen-1 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "w";
      };
    };

    throw-screen-2 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "e";
      };
    };

    throw-screen-3 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "r";
      };
    };

    throw-screen-4 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "q";
      };
    };

    shrink-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "h";
      };
    };

    expand-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "l";
      };
    };

    increase-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = ";";
      };
    };

    decrease-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = ".";
      };
    };

    focus-ccw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "j";
      };
    };

    focus-cw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "k";
      };
    };

    focus-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "m";
      };
    };

    swap-screen-ccw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "h";
      };
    };

    swap-screen-cw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "l";
      };
    };

    swap-ccw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "j";
      };
    };

    swap-cw = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "k";
      };
    };

    swap-main = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "enter";
      };
    };

    throw-space-1 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "1";
      };
    };

    throw-space-2 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "2";
      };
    };

    throw-space-3 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "3";
      };
    };

    throw-space-4 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "4";
      };
    };

    throw-space-5 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "5";
      };
    };

    throw-space-6 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "6";
      };
    };

    throw-space-7 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "7";
      };
    };

    throw-space-8 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "8";
      };
    };

    throw-space-9 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "9";
      };
    };

    throw-space-10 = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "0";
      };
    };

    throw-space-left = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "left";
      };
    };

    throw-space-right = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "right";
      };
    };

    toggle-float = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "t";
      };
    };

    toggle-tiling = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "t";
      };
    };

    display-current-layout = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "i";
      };
    };

    reevaluate-windows = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod1";
        key = "z";
      };
    };

    toggle-focus-follows-mouse = mkOption {
      type = types.submodule keyboardShortcutOptions;
      default = {
        mod = "mod2";
        key = "x";
      };
    };

    floating = mkOption {
      type = with types; listOf str;
      default = [];
    };

    floating-is-blacklist = mkOption {
      type = types.bool;
      default = true;
    };

    float-small-windows = mkOption {
      type = types.bool;
      default = true;
    };

    mouse-follows-focus = mkOption {
      type = types.bool;
      default = false;
    };

    focus-follows-mouse = mkOption {
      type = types.bool;
      default = false;
    };

    enables-layout-hud = mkOption {
      type = types.bool;
      default = true;
    };

    enables-layout-hud-on-space-change = mkOption {
      type = types.bool;
      default = true;
    };

    window-margin-size = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    window-resize-step = mkOption {
      type = types.ints.unsigned;
      default = 5;
    };

    window-margins = mkOption {
      type = types.bool;
      default = false;
    };

    window-minimum-height = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    window-minimum-width = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    ignore-menu-bar = mkOption {
      type = types.bool;
      default = false;
    };

    use-canary-build = mkOption {
      type = types.bool;
      default = false;
    };

    new-windows-to-main = mkOption {
      type = types.bool;
      default = false;
    };

    follow-space-thrown-windows = mkOption {
      type = types.bool;
      default = true;
    };

    send-crash-reports = mkOption {
      type = types.bool;
      default = false;
    };

    screen-padding-top = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    screen-padding-bottom = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    screen-padding-left = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    screen-padding-right = mkOption {
      type = types.ints.unsigned;
      default = 0;
    };

    debug-layout-info = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.shabka.darwinConfig != {};
        message = "Amethyst isn't used on Linux hosts. It should thus be disabled on them.";
      }
    ];

    home.file.".amethyst".text = builtins.toJSON
      (lib.filterAttrsRecursive (n: _: n != "enable" && n != "_module") cfg);
  };
}
