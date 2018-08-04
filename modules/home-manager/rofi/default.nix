{ pkgs, ... }:

let
  i3Support = pkgs.stdenv.mkDerivation rec {
    name = "rofi-i3-support-${version}";
    version = "0.0.1";
    src = ./i3-support;
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      install -d -m755 $out/bin

      substitute $src/i3-move-container $out/bin/i3-move-container \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq

      substitute $src/i3-rename-workspace $out/bin/i3-rename-workspace \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq

      substitute $src/i3-switch-workspaces $out/bin/i3-switch-workspaces \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq

      chmod 755 $out/bin/*
    '';
  };
in {
  programs.rofi = {
    enable = true;

    extraConfig = ''
      rofi.modi: window,run,ssh,drun,i3Workspaces:${i3Support}/bin/i3-switch-workspaces,i3RenameWorkspace:${i3Support}/bin/i3-rename-workspace,i3MoveContainer:${i3Support}/bin/i3-move-container
    '';

    font = "SourceCodePro 9";

    theme = "Adapta-Nokto";
  };
}
