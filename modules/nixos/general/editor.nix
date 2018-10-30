{ pkgs, ... }:

{
  # set the EDITOR to neovim
  environment.variables.EDITOR = "nvim";

  environment.systemPackages = with pkgs; [
    neovim
  ];
}
