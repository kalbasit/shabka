{ config, pkgs, lib, ... }:

let
  /*configFile = pkgs.writeText "config.nix" ''
    { pkgs, ... }:

    {
      allowUnfree = true;

      packageOverrides = pkgs: {
        nur = pkgs.lib.recursiveUpdate
          (import ${inputs.nur.path} { inherit pkgs; })
          ({
            repos = {
              kalbasit = import ${inputs.kalbasit-nur.path} { inherit pkgs; };
              risson = import ${shabka.external.risson.nur.path} { inherit pkgs; };
            };
          });
      };
    }
  '';*/
  a = false;

in {
  nixpkgs.config = { pkgs, ... }: { allowUnfree = true; };

  #xdg.configFile."nixpkgs/config.nix".source = configFile;
}
