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

  swaySupport = pkgs.stdenv.mkDerivation rec {
    name = "rofi-sway-support-${version}";
    version = "0.0.1";
    src = ./sway-support;
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
      rofi.modi: window,run,ssh,drun,i3Workspaces:${i3Support}/bin/i3-switch-workspaces,i3RenameWorkspace:${i3Support}/bin/i3-rename-workspace,i3MoveContainer:${i3Support}/bin/i3-move-container,SwayWorkspaces:${swaySupport}/bin/sway-switch-workspaces,SwayRenameWorkspace:${swaySupport}/bin/sway-rename-workspace,SwayMoveContainer:${swaySupport}/bin/sway-move-container
    '';

    font = "SourceCodePro 9";

    theme = "Adapta-Nokto";
  };
}
