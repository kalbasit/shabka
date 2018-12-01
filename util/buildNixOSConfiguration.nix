{ ... }:

{
  buildNixOSConfiguration = conf: (import <nixpkgs/nixos> {
    configuration = conf;
  }).system;
}
