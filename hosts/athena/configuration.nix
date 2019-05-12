{
  imports = [
    ../../modules/darwin

    ./home.nix
  ];

  networking.hostName = "athena";

  time.timeZone = "America/Los_Angeles";

  mine.gnupg.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.fonts.enable = true;
}
