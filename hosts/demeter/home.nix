{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }:
  { lib, ... }:

  with lib;

  {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;

    mine.git.enable = true;
    mine.pijul.enable = true;
    mine.keybase.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.keyboard.layouts = [ "colemak" ];
  };
}
