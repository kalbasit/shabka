with import ../../util;

{
  # darwin = buildNixDarwinConfiguration ./darwin-configuration.nix;
  home = buildHomeManagerConfiguration ./home.nix;
}
