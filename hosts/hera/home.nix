{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }:
  { lib, ... }:

  with lib;

  {
    imports = [
      ../../modules/home
    ]
    ++ (optionals (builtins.pathExists ./../../secrets/home) (singleton ./../../secrets/home));

    mine.nixosConfig = nixosConfig;

    home.file.".gnupg/scdaemon.conf".text = ''
      reader-port Yubico YubiKey FIDO+CCID 01 00
      disable-ccid
      card-timeout 5
    '';

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
    mine.keyboard.layouts = [ "colemak" ];
    mine.workstation.enable = true;

    mine.workstation.autorandr.enable = true;

    programs.autorandr.profiles = {
      "default" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0006afed6300000000001b010495221378025925935859932926505400000001010101010101010101010101010101783780b470382e406c30aa0058c11000001a602c80b470382e406c30aa0058c11000001a000000fe005034443748804231353648414e000000000000410296001000000a010a202000ac";
        };

        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            gamma = "1.0:0.909:0.909";
            rate = "60.03";
          };
        };
      };

      "home" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0006afed6300000000001b010495221378025925935859932926505400000001010101010101010101010101010101783780b470382e406c30aa0058c11000001a602c80b470382e406c30aa0058c11000001a000000fe005034443748804231353648414e000000000000410296001000000a010a202000ac";
          DP-2 = "00ffffffffffff001e6de25a28530600071a0104a55022789eca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a01e4020316712309060749100403011f13595a12830100009f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000000000000000000000000000000000000000000000000000000000000000aa";
        };

        config = {
          eDP-1 = {
            enable = true;
            position = "0x360";
            mode = "1920x1080";
            gamma = "1.0:0.909:0.909";
            rate = "60.03";
          };

          DP-2 = {
            enable = true;
            primary = true;
            position = "1920x0";
            mode = "3440x1440";
            gamma = "1.0:0.909:0.909";
            rate = "59.97";
          };
        };
      };

      "work" = {
        fingerprint = {
          DP-2 = "00ffffffffffff0010acb8a04c3934322f1c0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00434656394e38424b3234394c0a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a202020202020011a02031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          DP-3 = "00ffffffffffff0010acbaa0554e3132331c010380342078ea0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00434656394e38434c32314e550a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a2020202020200152020322f14f9005040302071601141f12132021222309070765030c00100083010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e960006442100001800000000000000000000000000000000000000000082";
          eDP-1 = "00ffffffffffff0006afed6300000000001b010495221378025925935859932926505400000001010101010101010101010101010101783780b470382e406c30aa0058c11000001a602c80b470382e406c30aa0058c11000001a000000fe005034443748804231353648414e000000000000410296001000000a010a202000ac";
        };

        config = {
          eDP-1 = { enable = false; };

          DP-2 = {
            enable = true;
            position = "0x0";
            mode = "1920x1200";
            gamma = "1.0:0.909:0.909";
            rate = "59.95";
          };

          DP-3 = {
            enable = true;
            primary = true;
            position = "1920x0";
            mode = "1920x1200";
            gamma = "1.0:0.909:0.909";
            rate = "59.95";
          };
        };
      };
    };
  };
}
