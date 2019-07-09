{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.ssh;

in {
  options.mine.ssh = {
    enable = mkEnableOption "SSH configurations";

    privateSSHPath = mkOption {
      type = types.path;
      default = null;
      defaultText = ''
        The path to the private SSH module
      '';
    };
  };

  config = mkIf cfg.enable {

    programs.ssh = if (cfg.privateSSHPath != null) then import cfg.privateSSHPath else {
        enable = true;

        extraConfig = ''
          KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
          PubkeyAuthentication yes
          HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
          Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
          MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
        '';
    };
  };
}