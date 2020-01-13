{ config, pkgs, ... }:

let
  cfg = config.shabka.fzf;
in {
  options.shabka.fzf.enable = mkEnableOption "Enable fzf - a command-line fuzzy finder.";

  programs.fzf = {
    enable = cfg.enable;
    defaultCommand = ''(${pkgs.git}/bin/git ls-tree -r --name-only HEAD || ${pkgs.silver-searcher}/bin/ag --hidden --ignore .git -g "")'';
  };
}
