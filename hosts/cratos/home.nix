{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }:
  { lib, ... }:

  with lib;

  let
    enableEmail = userName == "yl" && builtins.pathExists /yl/private/network-secrets/shabka/email.nix;
    enableSSH = builtins.pathExists /yl/private/network-secrets/shabka/ssh.nix;
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
          eDP-1 = "00ffffffffffff0006af2d5b00000000001c0104a51d107803ee95a3544c99260f505400000001010101010101010101010101010101b43780a070383e403a2a350025a21000001a902c80a070383e403a2a350025a21000001a000000fe003036564736804231333348414e0000000000024122a8011100000a010a202000f1";
        };

        config = {
          eDP-1 = {
            enable = true;
            position = "0x0";
            mode = "1920x1080";
            gamma = "1.0:0.909:0.909";
            rate = "60.03";
          };
        };
      };

      "work" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0006af2d5b00000000001c0104a51d107803ee95a3544c99260f505400000001010101010101010101010101010101b43780a070383e403a2a350025a21000001a902c80a070383e403a2a350025a21000001a000000fe003036564736804231333348414e0000000000024122a8011100000a010a202000f1";
          DP-1 = "00ffffffffffff0010aceb40535343410d1c0103803c22782aee95a3544c99260f5054a54b00714fa9408180d1c00101010101010101565e00a0a0a029503020350055502100001a000000ff003637594756383351414353530a000000fc0044454c4c205532373137440a20000000fd00324b1e5819000a2020202020200174020324f14f90050403020716010611121513141f23091f078301000067030c0010000032023a801871382d40582c450055502100001e7e3900a080381f4030203a0055502100001a011d007251d01e206e28550055502100001ebf1600a08038134030203a0055502100001a00000000000000000000000000000000000000b2";
        };

        config = {
          eDP-1 = {
            enable = true;
            position = "0x0";
            mode = "1920x1080";
            gamma = "1.0:0.909:0.909";
            rate = "60.03";
          };

          DP-1 = {
            enable = true;
            position = "1920x0";
            mode = "2560x1440";
            gamma = "1.0:0.909:0.909";
            rate = "59.95";
          };
        };
      };
    };

    mine.ssh = mkIf enableSSH {
      enable = true;
      privateSSHPath = /yl/private/network-secrets/shabka/ssh.nix;
    };
  };
}
