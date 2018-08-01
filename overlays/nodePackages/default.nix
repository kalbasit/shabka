self: super:

let
  nodePackages_v8 = import ./composition-v8.nix {};
in

{
  nodePackages = super.nodePackages // nodePackages_v8;
}
