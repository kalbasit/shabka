{ config, pkgs, lib, ... }:

with lib;

let
  i3Support = pkgs.stdenvNoCC.mkDerivation rec {
    name = "rofi-i3-support-${version}";
    version = "0.0.1";
    src = ./i3-support;
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      install -d -m755 $out/bin $out/lib

      substitute $src/i3-move-container.sh $out/bin/i3-move-container \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
        --subst-var-by out_dir $out

      substitute $src/i3-rename-workspace.sh $out/bin/i3-rename-workspace \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
        --subst-var-by out_dir $out

      substitute $src/i3-switch-workspaces.sh $out/bin/i3-switch-workspaces \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
        --subst-var-by out_dir $out

      substitute $src/list-workspaces.sh $out/lib/list-workspaces.sh \
        --subst-var-by i3-msg_bin ${pkgs.i3}/bin/i3-msg \
        --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
        --subst-var-by out_dir $out

      chmod 755 $out/bin/*
    '';
  };

  cfg = config.shabka.workstation.rofi;
in {
  options.shabka.workstation.rofi = {
    enable = mkEnableOption "workstation.rofi";

    dpi = mkOption {
      type = with types; nullOr ints.positive;
      default = null;
      description = "The DPI of the rofi.";
    };
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;

      extraConfig = (''
        rofi.modi: window,run,ssh,drun,i3Workspaces:${i3Support}/bin/i3-switch-workspaces,i3RenameWorkspace:${i3Support}/bin/i3-rename-workspace,i3MoveContainer:${i3Support}/bin/i3-move-container
      '') + (optionalString (cfg.dpi != null) ''
        rofi.dpi: ${builtins.toString cfg.dpi}
      '');

      font = "Source Code Pro for Powerline 9";
    };
  };
}
