let

  homeManager = import ../../../external/home-manager.nix;

in {
  imports = [
    (import homeManager {}).nixos
  ];
}
