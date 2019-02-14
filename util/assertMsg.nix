{ lib }:

# If this is failing the assertion, it means that it's now coming from lib.
# Once this happens, remove this file and fix any build errors by replacing
# import of util with import of nixpkgs.lib.
# TODO: bring back this assertion if release is stable
# assert ! builtins.isFunction (lib.assertMsg or null);

{
  assertMsg = pred: msg:
    if pred
    then true
    else builtins.trace msg false;
}
