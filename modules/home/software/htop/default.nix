{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.htop;
in {
  options.mine.htop.enable = mkOption {
    default = true;
    description = ''Enable htop'';
  };

  config = mkIf cfg.enable {

    programs.htop = {  # TODO: order those options in a way that makes sense
      enable = true;

      colorScheme = 0;
      headerMargin = false;

      highlightBaseName = true;
      highlightMegabytes = false;
      highlightThreads = true;
      showProgramPath = false;
      showThreadNames = false;

      delay = 15;
      cpuCountFromZero = false;
      detailedCpuTime = false;
      hideKernelThreads = false;
      hideUserlandThreads = false;
      shadowOtherUsers = false;
      updateProcessNames = false;

      fields = [ "PID" "USER" "PRIORITY" "NICE" "M_SIZE" "M_RESIDENT" "M_SHARE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
      meters = {
        left = [ "LeftCPUs" "Memory" "Swap" "CPU" ];
        right = [ "RightCPUs" "Tasks" "LoadAverage" "Uptime" ];
      };

      sortDescending = true;
      sortKey = "PERCENT_CPU";
      treeView = false;
    };
  };
}
