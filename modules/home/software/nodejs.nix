{ config, ... }:

{
  home.file.".npmrc".text = "prefix=${config.home.homeDirectory}/.filesystem";
}