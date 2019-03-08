{ callPackage }:

{
  release-18-09 = callPackage ./18.09 { };
  release-unstable = callPackage ./unstable { };
}
