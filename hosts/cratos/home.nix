let
  scaleBy40P = ''
    # scale by 40%
    xrandr --output eDP-1 --mode 3200x1800 --scale 0.6x0.6
  '';
in {
  mine.home-manager.config = { userName, uid, isAdmin, nixosConfig }: { ... }: {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.keybase.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.useColemakKeyboardLayout = true;
    mine.workstation.enable = true;

    xsession.initExtra = if nixosConfig != null && nixosConfig.networking.hostName == "cratos" then scaleBy40P else "";
  };
}
