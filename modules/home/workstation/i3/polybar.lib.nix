{ config, pkgs, lib, ... }:

let
  launchScript = ''
    # Terminate already running bar instances
    ${pkgs.procps}/bin/pkill polybar

    # Wait until the processes have been shut down
    while ${pkgs.procps}/bin/pgrep polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; done

    if type "xrandr"; then
      for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | ${pkgs.coreutils}/bin/cut -d" " -f1); do
        MONITOR=$m polybar --reload top &
      done
    else
      polybar --reload top &
    fi

    echo "Bars launched..."
  '';
in {
  enable = config.mine.workstation.i3.bar == "polybar";
  package = pkgs.polybar.override {
    i3Support = true;
    pulseSupport = true;
  };
  config = import ./polybar-config.lib.nix { inherit config pkgs lib; };
  script = launchScript;
}