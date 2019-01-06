{ pkgs, ... }:

{
  enable = true;
  config = ./polybar.config;
  script = ''
    for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | grep " connected" | cut -d" " -f1); do
      MONITOR=$m polybar --reload mainbar-i3 &
    done
  '';
  # config = {
  #
  #   # https://github.com/jaagr/polybar/wiki/Configuration#global-wm-settings
  #   "global/wm" = {
  #     "margin-top" = 0;
  #     "margin-bottom" = 0;
  #   };
  #
  #   # https://github.com/jaagr/polybar/wiki/Configuration#application-settings
  #   settings = {
  #     "throttle-output" = 5;
  #     "throttle-output-for" = 10;
  #     "throttle-input-for" = 30;
  #     "screenchange-reload" = true;
  #     "compositing-background" = "over";
  #     "compositing-foreground" = "over";
  #     "compositing-overline" = "over";
  #     "compositing-underline" = "over";
  #     "compositing-border" = "over";
  #
  #     # Define fallback values used by all module formats
  #     "format-foreground" = "#FF0000";
  #     "format-background" = "#00FF00";
  #     "format-underline" = "";
  #     "format-overline" = "";
  #     "format-spacing" = "";
  #     "format-padding" = "";
  #     "format-margin" = "";
  #     "format-offset" = "";
  #   };
  #
  #   # TODO: move these to the theme folder
  #   colors = {
  #     # Nord theme ============
  #     "background" = "#1D2330";
  #     "foreground" = "#c0c5ce";
  #     "alert" = "#bd2c40";
  #     "volume-min" = "#a3be8c";
  #     "volume-med" = "#ebcb8b";
  #     "volume-max" = "#bf616a";
  #
  #     # Gotham theme ==========
  #     # background = #0a0f14
  #     # foreground = #99d1ce
  #     # alert = #d26937
  #     # volume-min = #2aa889
  #     # volume-med = #edb443
  #     # volume-max = #c23127
  #     # =======================
  #
  #     # INTRCPTR theme ============
  #     #background = ${xrdb:color0:#222}
  #     #background = #aa000000
  #     #background-alt = #444
  #     #foreground = ${xrdb:color7:#222}
  #     #foreground = #fff
  #     #foreground-alt = #555
  #     #primary = #ffb52a
  #     #secondary = #e60053
  #     #alert = #bd2c40
  #   };
  # };
}
