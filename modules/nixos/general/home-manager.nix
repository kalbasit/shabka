let

  homeManager = let
    pinnedVersion = builtins.fromJSON (builtins.readFile ../../../external/home-manager-version.json);
    pinnedHM = builtins.fetchGit {
      inherit (pinnedVersion) url rev;
    };
  in
    pinnedHM;

in {
  imports = [
    (import homeManager {}).nixos
  ];
}
