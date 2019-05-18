{ config, pkgs, lib, userName, ... }:

with lib;

let
  cfg = config.mine.git;
in {
  options = {

    mine.git = {
      enable = mkEnableOption "git";

      userName = mkOption {
        type = types.str;
        default = "";
        description = "git user name";
      };

      userEmail = mkOption {
        type = types.str;
        default = "";
        description = "git user email";
      };

      gpgSigningKey = mkOption {
        type = types.str;
        default = "";
        description = "git PGP signing key";
      };
    };
  };

  config = mkIf cfg.enable (if (pathExists (./. + "/${userName}")) then import ./. + "/${userName}" else {

    programs.git = {
      enable = true;

      userName = cfg.userName;
      userEmail = icfg.userEmail;

      signing = mkIf cfg.gpgSigningKey != "" {
        key = cfg.gpgSigningKey;
        signByDefault = true;
      };
    };

  });
}
