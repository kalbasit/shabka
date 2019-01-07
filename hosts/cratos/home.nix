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
            mode = "1920x1080";
            gamma = "1.0:0.909:0.909";
            rate = "60.03";
          };
        };
      };

      "home" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff004d104a14000000001e190104a51d11780ede50a3544c99260f505400000001010101010101010101010101010101cd9180a0c00834703020350026a510000018a47480a0c00834703020350026a510000018000000fe0052584e3439814c513133335a31000000000002410328001200000b010a202000cc";
          DP-2 = "00ffffffffffff001e6de25a28530600071a0104a55022789eca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a01e4020316712309060749100403011f13595a12830100009f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000000000000000000000000000000000000000000000000000000000000000aa";
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

          DP-2 = {
            enable = true;
            primary = true;
            position = "3200x0";
            mode = "3440x1440";
            gamma = "1.0:0.909:0.909";
            rate = "59.97";
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
