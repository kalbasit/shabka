{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.autorandr;
in {
  options.mine.workstation.autorandr = {
    enable = mkEnableOption "Enable autorandr";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;
      profiles = {
        "single" = {
          fingerprint = {
            eDP-1 = "00ffffffffffff0006afed6000000000001a0104952213780232a5a555529d260b505400000001010101010101010101010101010101143780a670382e406c30aa0058c11000001a102c80a670382e406c30aa0058c11000001a000000fe004a52385033804231353648414e00000000000041029e001000000a010a2020001b";
          };

          config = {
            eDP-1 = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = "1.0:0.769:0.556";
              rate = "60.03";
            };
          };
        };

        "home" = {
          fingerprint = {
            eDP-1 = "00ffffffffffff0006afed6000000000001a0104952213780232a5a555529d260b505400000001010101010101010101010101010101143780a670382e406c30aa0058c11000001a102c80a670382e406c30aa0058c11000001a000000fe004a52385033804231353648414e00000000000041029e001000000a010a2020001b";
            DP-2 = "00ffffffffffff001e6de25a28530600071a0104a55022789eca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a01e4020316712309060749100403011f13595a12830100009f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000000000000000000000000000000000000000000000000000000000000000aa";
          };

          config = {
            eDP-1 = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = "1.0:0.769:0.556";
              rate = "60.03";
            };

            DP-2 = {
              enable = true;
              primary = true;
              position = "1920x0";
              mode = "3440x1440";
              gamma = "1.0:0.769:0.556";
              rate = "59.97";
            };
          };
        };
      };
    };
  };
}
