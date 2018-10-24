{
  mine.home-manager.users.kalbasit = { ... }: {
    imports = [
      ../../modules/home
    ];

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.gnupg.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.workstation.enable = true;
  };
}
