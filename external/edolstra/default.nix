{ mkExternal }:

{
  dwarffs = import ./dwarffs { inherit mkExternal; };
  flake-compat = import ./flake-compat { inherit mkExternal; };
}
