{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.gtk.enable = mkEnableOption "Enable GTK";

  config = mkIf config.shabka.workstation.gtk.enable {

    assertions = [{
      assertion = config.shabka.nixosConfig == {} || config.shabka.nixosConfig.shabka.workstation.gtk.enable;
      message = "If you enable GTK in home-manger, then you must also enable it in NixOS configuration";
    }];

    # configure GTK icon theme to fix missing icons issue
    # https://github.com/NixOS/nixpkgs/issues/32730#issuecomment-368310621
    gtk = {
      enable = true;
      iconTheme = { package = pkgs.hicolor_icon_theme; name = "hicolor"; };
    };
  };
}
