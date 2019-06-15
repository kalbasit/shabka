{ config, pkgs, lib, ... }:

with lib;

let
  colemakKeybindings = ''
    n left-scroll
    e forw-line
    i back-line
    o right-scroll
    k repeat-search
    K reverse-search
  '';

  lessKey = ''
    # NOTE: use --quit-if-one-screen only if --no-init is set. Otherwise less will
    # print nothing and exits which looks like it's crashing.
    LESS= --RAW-CONTROL-CHARS --no-init --quit-if-one-screen --ignore-case
  ''
  + optionalString ((builtins.head config.mine.keyboard.layouts) == "colemak") colemakKeybindings;

in {
  options.mine.less.enable = mkEnableOption "less";

  config = mkIf (config.mine.less.enable) {
    home.file.".less".text = lessKey;
  };
}
