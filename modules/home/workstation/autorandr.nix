{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.autorandr;
in {
  options.mine.workstation.autorandr = {
    enable = mkEnableOption "Enable autorandr";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;

      hooks = {
         postswitch = {
          "move-workspaces-to-main" = ''
            set -euo pipefail

            # Make sure that i3 is running
            if [[ "$( i3-msg -t get_outputs | jq -r '.[] | select(.active == true) | .name' | wc -l )" -eq 1 ]]; then
              echo "no other monitor, bailing out"
            fi

            # Figure out the identifier of the main monitor
            readonly main_monitor="$( i3-msg -t get_outputs | jq -r '.[] | select(.name != "eDP-1" and .active == true) | .name' )"

            # Get the list of workspaces that are not on the main monitor
            readonly workspaces=( $(i3-msg -t get_workspaces | jq -r '.[] | select(.output != "DP-2") | .name') )

            # Move all workspaces over
            for workspace in "''${workspaces[@]}"; do
              i3-msg "workspace ''${workspace}; move workspace to output ''${main_monitor}"
            done

            # Move the Slack workspace to the internal screen
            i3-msg "workspace slack; move workspace to output eDP-1"

            # Go to my personal workspace
            i3-msg "workspace personal@base"
          '';
        };
      };
    };
  };
}
