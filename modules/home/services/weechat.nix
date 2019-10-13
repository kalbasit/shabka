{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.shabka.weechat;
in {
  options.shabka.weechat = {
    enable = mkEnableOption "Enable Weechat Service";
  };

  config = mkIf cfg.enable {
    systemd.user.services.weechat = {
      Unit = {
        Description = "Persistant weechat on tmux";
        After = [ "network.target" ];
      };

      Service = {
        Type = "forking";
        RemainAfterExit = "yes";
        Restart = "on-failure";
        ExecStart = "${pkgs.tmux}/bin/tmux new-session -s weechat -d ${pkgs.weechat}/bin/weechat";
        ExecStop = "${pkgs.tmux}/bin/tmux kill-session -t weechat";
      };

      Install = {
        WantedBy = [ "multi-user.target" ];
      };
    };
  };
}