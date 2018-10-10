{ pkgs, ... }:

{
  # configure GTK icon theme to fix missing icons issue
  # https://github.com/NixOS/nixpkgs/issues/32730#issuecomment-368310621
  gtk = {
    enable = true;
    iconTheme = { package = pkgs.hicolor_icon_theme; name = "hicolor"; };
  };
}
