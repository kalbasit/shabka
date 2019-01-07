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
    mine.workstation.autorandr.monitor =  "00ffffffffffff0006afed6000000000001a0104952213780232a5a555529d260b505400000001010101010101010101010101010101143780a670382e406c30aa0058c11000001a102c80a670382e406c30aa0058c11000001a000000fe004a52385033804231353648414e00000000000041029e001000000a010a2020001b";

    mine.ssh = mkIf enableSSH {
      enable = true;
      privateSSHPath = /yl/private/network-secrets/shabka/ssh.nix;
    };

    mine.workstation.email = mkIf enableEmail {
      enable = true;
      privateEmailPath = /yl/private/network-secrets/shabka/email.nix;
    };
  };
}
