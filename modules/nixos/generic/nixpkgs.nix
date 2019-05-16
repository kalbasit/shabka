{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.nixpkgs;
  shabka = import <shabka> { };
in
{

  options.mine.nixpkgs = {
    extraRepo = types.submodule {
      options = {
        path = mkOption {
          type = types.path;
        };
      };
    };

    extraRepos = mkOption {
      type = types.attrs;
      default = { };
      example = { kalbasit = import shabka.external.kalbasit.nur.path { inherit pkgs; }; }; #TODO: make this our ex user's repo
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = recursiveUpdate
        (import shabka.external.nur.path { inherit pkgs; })
        ({
          repos = {
            #TODO: take the configuration option from up there and put it here #FIXME
          };
        });
    };
  };

  nixpkgs.overlays = import ../../../overlays;
}
