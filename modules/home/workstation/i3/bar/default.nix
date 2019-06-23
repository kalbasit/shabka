{ config, pkgs, lib, ... }:

{
  options.mine.workstation.i3 = {
    enable = mkEnableOption "workstation.i3";

    bar = {
      i3bar.enable = mkEnableOption "Enable workstation.i3.bar.i3bar";
      polybar.enable = mkEnableOption "Enable workstation.i3.bar.polybar";
    };
  };

  config = mkIf config.mine.workstation.i3.enable {
    assertions = [
      {
        assertion = cfg.bar.i3bar.enable != cfg.bar.i3bar.polybar;
        message = "i3bar and polybar cannot be used at the same time.";
      }
    ];

    services.polybar = (mkIf config.mine.workstation.i3.bar.polybar.enable import ./polybar.lib.nix { inherit config pkgs lib; });
    xdg.configFile."i3status/config" = (mkIf config.mine.workstation.i3.bar.i3bar.enable import ./i3status.lib.nix { inherit config pkgs lib; });
  };
}