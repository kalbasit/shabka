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

    mine.workstation.autorandr.enable = true;

    programs.autorandr.profiles = {
      "default" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff004d104a14000000001e190104a51d11780ede50a3544c99260f505400000001010101010101010101010101010101cd9180a0c00834703020350026a510000018a47480a0c00834703020350026a510000018000000fe0052584e3439814c513133335a31000000000002410328001200000b010a202000cc";
        };

        config = {
          eDP-1 = {
            enable = true;
            position = "0x0";
            mode = "3200x1800";
            gamma = "1.0:0.909:0.909";
            rate = "59.98";
            transform = "0.599991,0.000000,0.000000,0.000000,0.599991,0.000000,0.000000,0.000000,1.000000";
          };
        };
      };
    };

    mine.ssh = mkIf enableSSH {
      enable = true;
      privateSSHPath = /yl/private/network-secrets/shabka/ssh.nix;
    };

    xsession.initExtra = if nixosConfig != null then scaleBy40P else "";
  };
}
