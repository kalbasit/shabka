{ pkgs, ... }:

# TODO: convert this to a full module
{
  home.file.".config/alacritty/alacritty.yml".text = builtins.readFile ./alacritty.yml;
  home.packages = with pkgs; [ alacritty ];
}
