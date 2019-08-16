{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.gtk.enable = mkEnableOption "Enable GTK";

  config = mkIf config.shabka.workstation.gtk.enable {
    # configure GTK icon theme to fix missing icons issue
    # https://github.com/NixOS/nixpkgs/issues/32730#issuecomment-368310621
    gtk = {
      enable = true;
      iconTheme = { package = pkgs.hicolor_icon_theme; name = "hicolor"; };
    };
  };
}
