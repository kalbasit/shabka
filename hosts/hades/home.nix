{
  home-manager.users.kalbasit = { ... }: {
    imports = [
      ../../modules/home
    ];

    mine.workstation.enable = true;
    mine.timewarrior.enable = true;
    mine.batteryNotifier.enable = true;
  };
}
