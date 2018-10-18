{ config, pkgs, ... }:

let

  pinnedKalbasit = import ../../../external/kalbasit-nur.nix;

  configFile = pkgs.writeText "config.nix" ''
    {
      allowUnfree = true;

      packageOverrides = pkgs: {
        nur = {
          kalbasit = import ${pinnedKalbasit} { inherit pkgs; };
        };
      };

      chromium = {
        enablePepperFlash = true;
      };
    }
  '';

in {
  nixpkgs.config = {
    allowUnfree = true;

    packageOverrides = pkgs: {
      nur = {
        kalbasit = import pinnedKalbasit { inherit pkgs; };
      };
    };

    chromium = {
      enablePepperFlash = true;
    };
  };

  xdg.configFile."nixpkgs/config.nix".source = configFile;

  nixpkgs.overlays = import ../../../overlays;
}
