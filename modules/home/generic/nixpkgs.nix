{ config, pkgs, lib, ... }:

let

  pinnedNUR = import ../../../external/nur.nix;
  pinnedKalbasitNUR = import ../../../external/kalbasit-nur.nix;

  configFile = pkgs.writeText "config.nix" ''
    { pkgs, ... }:

    {
      allowUnfree = true;

      packageOverrides = pkgs: {
        nur = pkgs.lib.recursiveUpdate
          (import ${pinnedNUR} { inherit pkgs; })
          ({
            repos = {
              kalbasit = import ${pinnedKalbasitNUR} { inherit pkgs; };
            };
          });
      };

      chromium = {
        enablePepperFlash = true;
      };
    }
  '';

in {
  nixpkgs.config = { pkgs, ... }:
    {
      allowUnfree = true;

      packageOverrides = pkgs: {
        nur = pkgs.lib.recursiveUpdate
          (import pinnedNUR { inherit pkgs; })
          ({
            repos = {
              kalbasit = import pinnedKalbasitNUR { inherit pkgs; };
            };
          });
      };

      chromium = {
        enablePepperFlash = true;
      };
    };

  xdg.configFile."nixpkgs/config.nix".source = configFile;

  nixpkgs.overlays = import ../../../overlays;
}
