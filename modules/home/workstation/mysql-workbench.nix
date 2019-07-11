{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.workstation.mysql-workbench.enable = mkEnableOption "Enable Mysql Workbench";

  config = mkIf config.shabka.workstation.mysql-workbench.enable {
    home.packages = with pkgs; [
      mysqlWorkbench
    ];
  };
}
