{ lib, ... }:

with lib;

{
  # XXX: The checks fail when the nix-darwin does not find the configuration at
  # ~/.nixpkgs/darwin-configuration.nix. We force it to empty string to disable
  # these checks. Please see https://github.com/LnL7/nix-darwin/issues/117 for
  # more information.
  system.checks.text = mkForce "";
}
