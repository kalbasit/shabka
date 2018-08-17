{ pkgs, ... }:

{
  home.packages = with pkgs; [
    taskwarrior
    tasksh
  ];

  home.file.".taskrc".text = builtins.readFile (pkgs.substituteAll {
    src = ./taskrc;

    taskwarrior_out = "${pkgs.taskwarrior}";
  });

  home.file.".task/hooks/on-modify.timewarrior".source = "${pkgs.timewarrior}/share/doc/timew/ext/on-modify.timewarrior";
}
