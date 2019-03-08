{ callPackage, mkExternal }:

{
  keys = callPackage ./keys { };
  nur = callPackage ./nur { inherit mkExternal; };
}
