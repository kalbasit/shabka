{ config, pkgs, lib, ... }:

let
  cfg = config.mine.keyboard;

  layouts = {
    azerty = {
      layout = "fr";
      variant = "";
    };
    bepo = {
      layout = "fr";
      variant = "bepo";
    };
    colemak = {
      layout = "us";
      variant = "colemak";
    };
    qwerty = {
      layout = "us";
      variant = "";
    };
    qwerty_intl = {
      layout = "us";
      variant = "intl";
    };
  };
in

with lib;

{
  options = {
    mine.keyboard = {
      layouts = mkOption {
        type = types.listOf (types.enum (attrNames layouts));
        default = [ "qwerty" ];
        example = [ "colemak" "qwerty" ];
        description = "Layouts to set";
      };
    };
  };

  config = {
    home.keyboard.layout = builtins.concatStringsSep "," (map (n: layouts.n.layout) cfg.layouts);
    home.keyboard.variant = builtins.concatStringsSep "," (map (n: layouts.n.variant) cfg.layouts);
  };
}