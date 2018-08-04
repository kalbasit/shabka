{ pkgs, ... }:

{
  home.file.".taskrc".text = builtins.readFile (pkgs.substituteAll {
    src = ./taskrc;

    taskwarrior_out = "${pkgs.taskwarrior}";
  });
}
