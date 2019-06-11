{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.htop.enable = mkEnableOption "Enable htop";

  config = mkIf config.mine.htop.enable {

    programs.htop = {  # TODO: order those options in a way that makes sense
      enable = true;

      colorScheme = 0;
      cpuCountFromZero = false;
      delay = 15;
      detailedCpuTime = false;
      fields = [ "PID" "USER" "PRIORITY" "NICE" "M_SIZE" "M_RESIDENT" "M_SHARE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
      headerMargin = false;
      hideKernelThreads = false;
      hideUserlandThreads = false;
      highlightBaseName = true;
      highlightMegabytes = false;
      highlightThreads = true;
      meters = {
        left = [ "LeftCPUs" "Memory" "Swap" "CPU" ];
        right = [ "RightCPUs" "Tasks" "LoadAverage" "Uptime" ];
      };
      shadowOtherUsers = false;
      showProgramPath = false;
      showThreadNames = false;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
      treeView = false;
      updateProcessNames = false;
    };

  };
}