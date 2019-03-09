{ mkExternal }:

{
  keys = import ./keys { };
  nur = import ./nur { inherit mkExternal; };
}
