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

      enableAtBoot = mkEnableOption "If true, the first of the layouts will be enabled at boot";
    };
  };

  config = {
    boot.earlyVconsoleSetup = cfg.enableAtBoot;
    i18n.consoleKeyMap = let firstLayout = builtins.head cfg.layouts; in
                         if layouts.firstLayout.variant == ""
                         then layouts.firstLayout.layout
                         else layouts.firstLayout.variant;
    services.xserver.layout = builtins.concatStringsSep "," (map (n: layouts.n.layout) cfg.layouts);
    services.xserver.xkbVariant = builtins.concatStringsSep "," (map (n: layouts.n.variant) cfg.layouts);
  };
}