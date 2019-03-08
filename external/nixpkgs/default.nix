{ callPackage, mkExternal }:

{
  release-18-09 = callPackage ./18.09 { inherit mkExternal; };
  release-unstable = callPackage ./unstable { inherit mkExternal; };
}
