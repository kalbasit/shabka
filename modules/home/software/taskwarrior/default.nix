{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.taskwarrior.enable = mkEnableOption "taskwarrior";

  config = mkIf config.mine.taskwarrior.enable {
    programs.taskwarrior = {
      enable = true;
      colorTheme = "solarized-dark-256";

      extraConfig = ''
        # Urgency settings
        urgency.user.tag.bug.coefficient=5.0
        urgency.user.tag.problem.coefficient=4.5
        urgency.user.tag.later.coefficient=-6.0
        urgency.user.tag.waiting.coefficient=-12.0
        urgency.user.tag.backlog.coefficient=-20.0

        # Taskserver sync
        taskd.certificate=~/.task/taskserver.nasreddine.com/taskserver.nasreddine.com.crt
        taskd.key=~/.task/taskserver.nasreddine.com/taskserver.nasreddine.com.key
        taskd.ca=~/.task/taskserver.nasreddine.com/ca.crt
        taskd.server=taskserver.nasreddine.com:53589
        taskd.credentials=Home\/Wael Nasreddine\/92d86b3f-aa16-412b-bfff-0050e8a5a50c

        # UDA settings for tasksh
        uda.reviewed.type=date
        uda.reviewed.label=Reviewed

        # Report settings
        report._reviewed.description=Tasksh review report. Adjust the filter to your needs.
        report._reviewed.columns=uuid
        report._reviewed.sort=reviewed+,modified+
        report._reviewed.filter=( reviewed.none: or reviewed.before:now-1week ) and ( +PENDING or +WAITING )

        # Context settings
        context.publica=+publica
        context.personal=-publica
      '';
    };

    programs.zsh.shellAliases = {
      t = "task";

      eod = "task due:eod";
      tomorrow = "task due:sod";
      weekend = "task \\(due:saturday or due:sunday or due:mondayT00:00\\)";
    };

    # TODO: get tasksh compatible with Mac
    home.packages = if pkgs.stdenv.isLinux then [ pkgs.tasksh ] else [];
  };
}
