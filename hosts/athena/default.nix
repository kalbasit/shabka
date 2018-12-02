with import ../../util;

{
  athena = buildNixDarwinConfiguration ./darwin-configuration.nix;
  # athena-home = buildHomeManagerConfiguration ./home.nix;
}
