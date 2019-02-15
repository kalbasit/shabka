with import ../../util;

{
  athena = buildNixDarwinConfiguration ./configuration.nix;
  # athena-home = buildHomeManagerConfiguration ./home.nix;
}
