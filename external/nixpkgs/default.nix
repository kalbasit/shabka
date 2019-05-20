{ mkExternal }:

{
  release-19-03 = import ./19.03 { inherit mkExternal; };
  release-unstable = import ./unstable { inherit mkExternal; };
}
