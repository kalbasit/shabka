{
  mine.home-manager.config = { userName, uid, isAdmin, nixosConfig }: { ... }: {
    imports = [
      ../../modules/home
    ] ++ (if userName == "yl" then [
      /yl/private/network-secrets/shabka/email.nix
    ] else []);

    mine.nixosConfig = nixosConfig;

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.gnupg.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.useColemakKeyboardLayout = true;
    mine.workstation.enable = true;
  };
}
