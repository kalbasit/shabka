{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.ssh;

in {
  options.mine.ssh = {
    enable = mkEnableOption "SSH configurations";

    privateSSHPath = mkOption {
      type = types.path;
      defaultText = ''
        The path to the private SSH module
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists cfg.privateSSHPath;
        message = "privateSSHPath must exist";
      }
    ];

    programs.ssh = if cfg.enable then import cfg.privateSSHPath else {};
  };
}
