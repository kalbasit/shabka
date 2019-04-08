{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

let
  hostConf = host: "./hosts/${host}/configuration.nix";
  # hostRelease = (builtins.readFile ./hosts/${host}/.release) or (builtins.readFile ./.release)
  eval = system: configuration: import "${pkgs.path}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [ configuration ];
  };
in rec {
  # hades = (eval "x86_64-linux" /*(hostConf "hades")*/ ./hosts/hades/configuration.nix /* <- */).config.system.build.toplevel;

  jobs = {
    buildHades = import "${pkgs.path}/nixos/tests/make-test.nix" ({ pkgs, ... }: {
      machine =
        { ... }:
        { imports = [ ./hosts/hades/configuration.nix ]; };
        testScript =
          ''
          $machine->waitForX;
          $machine->sleep(10);
          $machine->screenshot("screen");
          '';
        });

      };
}
