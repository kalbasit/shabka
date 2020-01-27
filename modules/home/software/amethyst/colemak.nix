{ lib, config, ... }:

with lib;

{
  config.shabka.amethyst = mkIf (config.shabka.amethyst.keyboardLayout == "colemak") {
    cycle-layout = mkDefault {
      mod = "mod1";
      key = "space";
    };

    cycle-layout-backward = mkDefault {
      mod = "mod2";
      key = "space";
    };

    select-tall-layout = mkDefault {
      mod = "mod1";
      key = "a";
    };

    select-wide-layout = mkDefault {
      mod = "mod1";
      key = "s";
    };

    select-fullscreen-layout = mkDefault {
      mod = "mod1";
      key = "d";
    };

    select-column-layout = mkDefault {
      mod = "mod1";
      key = "f";
    };

    focus-screen-ccw = mkDefault {
      mod = "mod1";
      key = "p";
    };

    focus-screen-cw = mkDefault {
      mod = "mod1";
      key = "n";
    };

    focus-screen-1 = mkDefault {
      mod = "mod1";
      key = "w";
    };

    focus-screen-2 = mkDefault {
      mod = "mod1";
      key = "e";
    };

    focus-screen-3 = mkDefault {
      mod = "mod1";
      key = "r";
    };

    focus-screen-4 = mkDefault {
      mod = "mod1";
      key = "q";
    };

    throw-screen-1 = mkDefault {
      mod = "mod2";
      key = "w";
    };

    throw-screen-2 = mkDefault {
      mod = "mod2";
      key = "e";
    };

    throw-screen-3 = mkDefault {
      mod = "mod2";
      key = "r";
    };

    throw-screen-4 = mkDefault {
      mod = "mod2";
      key = "q";
    };

    shrink-main = mkDefault {
      mod = "mod1";
      key = "h";
    };

    expand-main = mkDefault {
      mod = "mod1";
      key = "l";
    };

    increase-main = mkDefault {
      mod = "mod1";
      key = ";";
    };

    decrease-main = mkDefault {
      mod = "mod1";
      key = ".";
    };

    focus-ccw = mkDefault {
      mod = "mod1";
      key = "j";
    };

    focus-cw = mkDefault {
      mod = "mod1";
      key = "k";
    };

    focus-main = mkDefault {
      mod = "mod1";
      key = "m";
    };

    swap-screen-ccw = mkDefault {
      mod = "mod2";
      key = "h";
    };

    swap-screen-cw = mkDefault {
      mod = "mod2";
      key = "l";
    };

    swap-ccw = mkDefault {
      mod = "mod2";
      key = "j";
    };

    swap-cw = mkDefault {
      mod = "mod2";
      key = "k";
    };

    swap-main = mkDefault {
      mod = "mod1";
      key = "enter";
    };

    throw-space-1 = mkDefault {
      mod = "mod2";
      key = "1";
    };

    throw-space-2 = mkDefault {
      mod = "mod2";
      key = "2";
    };

    throw-space-3 = mkDefault {
      mod = "mod2";
      key = "3";
    };

    throw-space-4 = mkDefault {
      mod = "mod2";
      key = "4";
    };

    throw-space-5 = mkDefault {
      mod = "mod2";
      key = "5";
    };

    throw-space-6 = mkDefault {
      mod = "mod2";
      key = "6";
    };

    throw-space-7 = mkDefault {
      mod = "mod2";
      key = "7";
    };

    throw-space-8 = mkDefault {
      mod = "mod2";
      key = "8";
    };

    throw-space-9 = mkDefault {
      mod = "mod2";
      key = "9";
    };

    throw-space-10 = mkDefault {
      mod = "mod2";
      key = "0";
    };

    throw-space-left = mkDefault {
      mod = "mod2";
      key = "left";
    };

    throw-space-right = mkDefault {
      mod = "mod2";
      key = "right";
    };

    toggle-float = mkDefault {
      mod = "mod1";
      key = "t";
    };

    toggle-tiling = mkDefault {
      mod = "mod2";
      key = "t";
    };

    display-current-layout = mkDefault {
      mod = "mod1";
      key = "i";
    };

    reevaluate-windows = mkDefault {
      mod = "mod1";
      key = "z";
    };

    toggle-focus-follows-mouse = mkDefault {
      mod = "mod2";
      key = "x";
    };
  };
}
