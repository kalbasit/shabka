with import ../../util;

{
  darwin = buildNixDarwinConfiguration ./configuration.nix;
  # home = buildHomeManagerConfiguration ./home.nix;
}
