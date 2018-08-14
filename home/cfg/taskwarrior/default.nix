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
}
