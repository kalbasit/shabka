{ pkgs, ... }:

{
  home.file.".surfingkeys.js".text = builtins.readFile (pkgs.substituteAll {
    src = ./surfingkeys.js;

    home_dir = "/home/kalbasit"; # TODO: set this from the config
  });
}
