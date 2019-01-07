{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.autorandr;
in {
  options.mine.workstation.autorandr = {
    enable = mkEnableOption "Enable autorandr";
    monitor = mkOption {
      description = "The EDID of the internal monitor";
      type = types.str;
    };
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

      profiles = {
        "default" = {
          fingerprint = {
            eDP-1 = cfg.monitor;
          };

          config = {
            eDP-1 = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = "1.0:0.909:0.909";
              rate = "60.03";
            };
          };
        };

        "home" = {
          fingerprint = {
            eDP-1 = cfg.monitor;
            DP-2 = "00ffffffffffff001e6de25a28530600071a0104a55022789eca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a01e4020316712309060749100403011f13595a12830100009f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000000000000000000000000000000000000000000000000000000000000000aa";
          };

          config = {
            eDP-1 = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = "1.0:0.909:0.909";
              rate = "60.03";
            };

            DP-2 = {
              enable = true;
              primary = true;
              position = "1920x0";
              mode = "3440x1440";
              gamma = "1.0:0.909:0.909";
              rate = "59.97";
            };
          };
        };
      };
    };
  };
}
