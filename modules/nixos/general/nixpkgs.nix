{ lib, ... }:

with lib;

{
  nixpkgs.config = { allowUnfree = true; };
}
