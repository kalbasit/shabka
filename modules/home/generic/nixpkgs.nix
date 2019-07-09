{ config, pkgs, lib, ... }:

let
  shabka = import <shabka> { };

  configFile = pkgs.writeText "config.nix" ''
    { pkgs, ... }:
    {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = pkgs.lib.recursiveUpdate
          (import ${shabka.external.nur.path} { inherit pkgs; })
          ({
            repos = {
              kalbasit = import ${shabka.external.kalbasit.nur.path} { inherit pkgs; };
            };
          });
      };
    }
  '';

in {
  nixpkgs.config = { pkgs, ... }:
    {
      allowUnfree = true;

      packageOverrides = pkgs: {
        nur = pkgs.lib.recursiveUpdate
          (import shabka.external.nur.path { inherit pkgs; })
          ({
            repos = {
              kalbasit = import shabka.external.kalbasit.nur.path { inherit pkgs; };
            };
          });
      };
    };

  xdg.configFile."nixpkgs/config.nix".source = configFile;

  nixpkgs.overlays = import ../../../overlays;
}
