{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }: { ... }: {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.gnupg.enable = true;
    mine.keybase.enable = true;
    mine.less.enable = true;
    mine.neovim.enable = true;
    mine.pet.enable = true;
    mine.ssh.enable = true;
    mine.ssh.privateSSHPath  = /yl/private/network-secrets/shabka/ssh.nix;
    mine.taskwarrior.enable = true;
    mine.timewarrior.enable = true;
    mine.tmux.enable = true;
    mine.useColemakKeyboardLayout = true;
    mine.workstation.enable = true;

    mine.workstation.email.enable = if userName == "yl" then true else false;
  };
}
