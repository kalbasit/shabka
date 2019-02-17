{ lib, ... }:

with lib;

{
  imports = [
    ../../modules/darwin

    ./home.nix
  ];

  networking.hostName = "athena";
}
