{
  mine.home-manager.config = { name, uid, isAdmin, nixosConfig }: { ... }: {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.gnupg.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.workstation.enable = true;

    mine.workstation.i3.enable = if name != "yl_presentation" then true else false;
  };
}
