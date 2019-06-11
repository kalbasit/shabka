{
  mine.home-manager.config = { userName, uid, isAdmin, home, nixosConfig }:
  { lib, ... }:

  with lib;

  {
    imports = [
      ../../modules/home
    ];

    mine.nixosConfig = nixosConfig;


    home.file.".gnupg/scdaemon.conf".text = ''
      reader-port Yubico YubiKey
      disable-ccid
      card-timeout 5
    '';

    mine.batteryNotifier.enable = true;
    mine.git.enable = true;
    mine.gnupg.enable = true;
    mine.htop.enable = true;
    mine.keybase.enable = true;
    mine.ssh.enable = true;
    mine.tmux.enable = true;
    mine.workstation = {
      enable = true;
      chromium.enable = true;
      firefox.enable = true;
      i3.enable = true;
      rofi.enable = true;
      urxvt.enable = true;
      bluetooth.enable = true;
      gtk.enable = true;
      locker.enable = true;
    };

    dconf.enable = true;
  };
}
