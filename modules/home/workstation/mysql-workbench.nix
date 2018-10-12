{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.mysql-workbench.enable = mkEnableOption "Enable Mysql Workbench";

  config = mkIf config.mine.workstation.mysql-workbench.enable {
    home.packages = with pkgs; [
      mysqlWorkbench
    ];
  };
}
