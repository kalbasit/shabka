{ pkgs, ... }:

let
  i3Support = pkgs.stdenv.mkDerivation rec {
    name = "rofi-i3-support-${version}";
    version = "0.0.1";
    src = ./i3-support;
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      install -d -m755 $out/bin
      cp -dr $src/* $out/bin
    '';
  };
in {
  programs.rofi = {
    enable = true;

    extraConfig = ''
      rofi.modi: window,run,ssh,drun,i3Workspaces:${i3Support}/bin/i3-switch-workspaces,i3RenameWorkspace:${i3Support}/bin/i3-rename-workspace,i3MoveContainer:${i3Support}/bin/i3-move-container,SwayWorkspaces:sway-switch-workspaces,SwayRenameWorkspace:sway-rename-workspace,SwayMoveContainer:sway-move-container
    '';

    font = "SourceCodePro 9";

    theme = "Adapta-Nokto";
  };
}
