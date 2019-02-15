{ lib, ... }:

with lib;

{
  imports = [
    ../../modules/darwin

    ./home.nix
  ];

  networking.hostName = "athena";

  # Disable all checks.
  system.checks.text = mkForce "";
}
