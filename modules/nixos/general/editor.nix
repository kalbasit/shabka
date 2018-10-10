{ pkgs, ... }:

{
  # install all completions libraries for system packages
  environment.pathsToLink = [ "/share/zsh" ];

  # set the EDITOR to neovim
  environment.variables.EDITOR = "nvim";

  environment.systemPackages = with pkgs; [
    neovim
  ];
}
