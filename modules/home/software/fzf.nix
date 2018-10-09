{ pkgs, ... }:

{
  # enable FZF
  programs.fzf = {
    enable = true;
    defaultCommand = ''
      (${pkgs.git}/bin/git ls-tree -r --name-only HEAD || ${pkgs.silver-searcher}/bin/ag --hidden --ignore .git -g "")
    '';
  };
}
