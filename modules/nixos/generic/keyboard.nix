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
        example = [ "bepo" "qwerty" ];
        description = "Layouts to set";
      };

      enableAtBoot = mkEnableOption "If true, the first of the layouts will be enabled at boot";
    };
  };

  config = {
    boot.earlyVconsoleSetup = cfg.enableAtBoot;
    i18n.consoleKeyMap = if layouts.(builtins.head cfg.layouts).variant == ""
                         then layouts.(builtins.head cfg.layouts).layout
                         else layouts.(builtins.head cfg.layouts).variant;
    services.xserver.layout = toString (map (n: layouts.n.layout) cfg.layouts);
    services.xserver.xkbVariant = builtins.concatStringsSep "," (map (n: layouts.n.variant) cfg.layouts);
  };
}