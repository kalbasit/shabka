{
  imports = [
    ../../modules/darwin

    ./home.nix
  ];

  networking.hostName = "athena";

  nix.buildMachines =
    if builtins.pathExists /Users/yl/private/network-secrets/shabka/hosts/zeus/id_rsa then
    [{
      hostName = "zeus.home.nasreddine.com";
      sshUser = "builder";
      sshKey = "/Users/yl/private/network-secrets/shabka/hosts/zeus/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    }] else [];
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  time.timeZone = "America/Los_Angeles";
}
