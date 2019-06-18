{ config, pkgs, lib, ... }:

let
  script = ''
    # Terminate already running bar instances
    kill $(${pkgs.procps}/bin/pgrep -f polybar | ${pkgs.gnugrep}/bin/grep -v ^$$\$)

    # Wait until the processes have been shut down
    while ${pkgs.procps}/bin/pgrep -f polybar | ${pkgs.gnugrep}/bin/grep -v ^$$\$ >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; done

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
  enable = config.mine.workstation.i3.bar.engine == "polybar";
  package = pkgs.polybar.override {
    i3Support = true;
    pulseSupport = true;
  };
  config = import ./polybar-config.lib.nix { inherit config pkgs lib; };
  inherit script;
}