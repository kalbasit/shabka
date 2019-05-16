with import ../../util;

{
  nixos = buildNixDarwinConfiguration ./configuration.nix;
  # home = buildHomeManagerConfiguration ./home.nix;
}
