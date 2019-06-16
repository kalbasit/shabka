{
  mine.home-manager.config = { darwinConfig }:
  { ... }:

  {
    imports = [
     ../../modules/home
    ];

    mine.darwinConfig = darwinConfig;

    mine.git.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.keyboard.layouts = [ "colemak" ];
  };
}
