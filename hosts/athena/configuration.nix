{
  imports = [
    ../../modules/darwin

    ./home.nix
  ];

  networking.hostName = "athena";

  nix.buildMachines = [
    (optionalAttrs (builtins.pathExists /Users/yl/private/network-secrets/shabka/hosts/demeter/id_rsa) {
      hostName = fileContents /Users/yl/private/network-secrets/shabka/hosts/demeter/hostname;
      sshUser = "builder";
      sshKey = "/Users/yl/private/network-secrets/shabka/hosts/demeter/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    })

    (optionalAttrs (builtins.pathExists /Users/yl/private/network-secrets/shabka/hosts/zeus/id_rsa) {
      hostName = "zeus.home.nasreddine.com";
      sshUser = "builder";
      sshKey = "/Users/yl/private/network-secrets/shabka/hosts/zeus/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    })
  ];
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  time.timeZone = "America/Los_Angeles";

  mine.gnupg.enable = true;
  mine.useColemakKeyboardLayout = true;
  mine.fonts.enable = true;
}
