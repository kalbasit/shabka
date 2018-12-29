{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }:
  { lib, ... }:

  with lib;

  let
    enableEmail = userName == "yl" && builtins.pathExists /yl/private/network-secrets/shabka/email.nix;
    enableSSH = builtins.pathExists /yl/private/network-secrets/shabka/ssh.nix;
    scaleBy40P = ''
      # scale by 40%
      xrandr --output eDP-1 --mode 3200x1800 --scale 0.6x0.6
    '';
  in {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.pijul.enable = true;
    mine.gnupg.enable = true;
    mine.keybase.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.useColemakKeyboardLayout = true;
    mine.workstation.enable = true;

    mine.ssh = mkIf enableSSH {
      enable = true;
      privateSSHPath = /yl/private/network-secrets/shabka/ssh.nix;
    };

    xsession.initExtra =
      if nixosConfig != null && nixosConfig.networking.hostName == "cratos"
      then scaleBy40P
      else "";
  };
}
