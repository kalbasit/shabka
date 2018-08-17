{ pkgs, ... }:

{
  home.packages = with pkgs; [
    timewarrior
    python # needed by totals.py extension
  ];

  home.file.".timewarrior/timewarrior.cfg".text = builtins.readFile (pkgs.substituteAll {
    src = ./timewarriorcfg;

    timewarrior_out = "${pkgs.timewarrior}";
  });

  home.file.".timewarrior/extensions/totals.py" = {
    source = "${pkgs.timewarrior}/share/doc/timew/ext/totals.py";
    executable = true;
  };
}
