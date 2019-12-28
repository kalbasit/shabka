{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.shabka.weechat;
in {
  options.shabka.weechat = {
    enable = mkEnableOption "Enable Weechat Service";

    sessionName = mkOption {
      default = "weechat";
      example = "irc";
      type = types.str;
      description = "The name of the tmux session weechat is ran in.";
    };
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
        ExecStart = "/bin/sh -c '" + (optionalString config.programs.tmux.secureSocket "TMUX_TMPDIR=${config.home.sessionVariables.TMUX_TMPDIR} ") + "${pkgs.tmux}/bin/tmux new-session -s ${cfg.sessionName} -d ${pkgs.weechat}/bin/weechat'";
        ExecStop = "/bin/sh -c '" + (optionalString config.programs.tmux.secureSocket "TMUX_TMPDIR=${config.home.sessionVariables.TMUX_TMPDIR} ") + "${pkgs.tmux}/bin/tmux kill-session -t ${cfg.sessionName}'";
      };

      Install = {
        WantedBy = [ "multi-user.target" ];
      };
    };
  };
}
