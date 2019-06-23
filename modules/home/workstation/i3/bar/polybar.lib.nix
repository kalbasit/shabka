{ config, pkgs, lib, ... }:

let
  script = ''
    for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | ${pkgs.coreutils}/bin/cut -d" " -f1); do
      echo "Starting polybar on monitor $m"
      MONITOR=$m polybar --reload top &
    done
  '';
in {
  enable = config.mine.workstation.i3.bar.polybar.enable;
  package = pkgs.polybar.override {
    i3Support = true;
    pulseSupport = true;
  };
  config = import ./polybar-config.lib.nix { inherit config pkgs lib; };
  inherit script;
}
