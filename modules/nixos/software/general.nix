{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    tmux mosh
  ];
}
