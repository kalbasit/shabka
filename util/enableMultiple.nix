{ lib }:

{
  enableMultiple = list:
    lib.genAttrs list (x: { enable = true; });
}
