{
  mine.home-manager.config = { userName, uid, isAdmin, home, darwinConfig }:
  { ... }:

  {
    imports = [
     ../../modules/home
    ];

    # TODO: getting an error that this is only compatible with systemd. A port to launchd is probably what I need.
    # mine.gnupg.enable = true;

    mine.darwinConfig = darwinConfig;

    mine.git.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.useColemakKeyboardLayout = true;
  };
}
