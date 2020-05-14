{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.autorandr;
in {
  options.shabka.workstation.autorandr = {
    enable = mkEnableOption "Enable autorandr";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;

      hooks = {
        postswitch = mkMerge [
          {
            "move-workspaces-to-main" = ''
              set -euo pipefail

              # Make sure that i3 is running
              if [[ "$( ${getBin pkgs.i3}/bin/i3-msg -t get_outputs | ${getBin pkgs.jq}/bin/jq -r '.[] | select(.active == true) | .name' | wc -l )" -eq 1 ]]; then
                echo "no other monitor, bailing out"
                exit 0
              fi

              # Figure out the identifier of the main monitor
              readonly main_monitor="$( ${getBin pkgs.i3}/bin/i3-msg -t get_outputs | ${getBin pkgs.jq}/bin/jq -r '.[] | select(.active == true and .primary == true) | .name' )"

              # Get the list of workspaces that are not on the main monitor
              readonly workspaces=( $(${getBin pkgs.i3}/bin/i3-msg -t get_workspaces | ${getBin pkgs.jq}/bin/jq -r '.[] | select(.output != "''${main_monitor}") | .name') )

              # Move all workspaces over
              for workspace in "''${workspaces[@]}"; do
                ${getBin pkgs.i3}/bin/i3-msg "workspace ''${workspace}; move workspace to output ''${main_monitor}"
              done

              # Move the Slack workspace to the internal screen
              ${getBin pkgs.i3}/bin/i3-msg "workspace slack; move workspace to output eDP-1"

              # Move the TV workspace to the internal screen
              ${getBin pkgs.i3}/bin/i3-msg "workspace tv; move workspace to output DP-3"

              # Go to my personal workspace
              ${getBin pkgs.i3}/bin/i3-msg "workspace personal@base"
            '';
          }

          (mkIf config.shabka.workstation.i3.bar.polybar.enable {
            "restart-polybar" = ''
              set -euo pipefail

              systemctl --user restart polybar.service
            '';
          })
        ];
      };
    };
  };
}
