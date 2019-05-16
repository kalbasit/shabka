{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.editor = mkOption {
    type = types.enum [ "nano" "vim" "nvim" "emacs" ];
    default = "vim";
  };

  # set the EDITOR to neovim
  config = {
    environment.variables.EDITOR = config.mine.editor;
  };
}
