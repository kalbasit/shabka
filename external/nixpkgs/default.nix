{ mkExternal }:

{
  release-19-09 = import ./19.09 { inherit mkExternal; };
  release-20-03 = import ./20.03 { inherit mkExternal; };
  release-unstable = import ./unstable { inherit mkExternal; };
}
