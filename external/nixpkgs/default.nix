{ mkExternal }:

{
  release-19-03 = import ./19.03 { inherit mkExternal; };
  release-19-09 = import ./19.09 { inherit mkExternal; };
  release-unstable = import ./unstable { inherit mkExternal; };
}
