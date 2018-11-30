{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.ssh;

in {
  options.mine.ssh.enable = mkEnableOption "SSH configurations";
  options.mine.ssh.privateSSHPath = mkOption {
    type = types.path;
    defaultText = ''
      The path to the private SSH module
    '';
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.privateSSHPath != null;
        message = "privateSSHPath is required";
      }
      {
        assertion = builtins.pathExists cfg.privateSSHPath;
        message = "privateSSHPath must exist";
      }
    ];

    programs.ssh = import cfg.privateSSHPath;
  };
}
