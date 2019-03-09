{ mkExternal }:

{
  release-18-09 = import ./18.09 { inherit mkExternal; };
  release-unstable = import ./unstable { inherit mkExternal; };
}
