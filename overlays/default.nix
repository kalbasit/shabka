{ ... }:

with import <nixpkgs/lib>;
with import ../util;

let
  stableFilters = ["default.nix" "unstable.*" "python.*" "nodePackages" "haskellPackages" "pkgs"];

  # TODO: it still goes into the nodePackages folder for some reason
  # stable = filteredModules stableFilters ./.;
  stable = [];

  unstable = [(self: super:
    let
      pinnedVersion = builtins.fromJSON (builtins.readFile ../external/nixpkgs-version.json);
      pinnedPkgs = builtins.fetchGit {
        inherit (pinnedVersion) url rev;
      };
    in {
      unstable = import pinnedPkgs {
        config = {};
        overlays = [];#python3 ++ (filteredModules [] ./unstable);
      };
    })];

  # TODO: this is erroring out because it can't find the python3 folder.
  python3 = [];
  # python3 = [(self: super: {
  #   python3 = super.python3.override (old:
  #     let
  #       po = old.packageOverrides or (self: super: {});
  #       pythonModules = filteredModules [] ./python3;
  #       pythonPackages = map (m: m self) pythonModules;
  #     in {
  #       packageOverrides =
  #         foldl' composeExtensions (self: super: {}) pythonPackages;
  #     }
  #   );
  # })];

  pkgs = [(self: super: recCallPackage ./pkgs)];

in {
  nixpkgs.overlays = pkgs ++ stable ++ unstable ++ [
    # TODO: move these to follow python3 above
    (import ./nodePackages)
    (import ./haskellPackages)

    # TODO once stable is fixed, remove these
    (import ./git-appraise.nix)
    (import ./timewarrior.nix)
  ];
}

