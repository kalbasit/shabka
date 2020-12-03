self: super:

let
  nx = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/23a5371532aed94099834e46058a7e307c3dda87.tar.gz";
    sha256 = "0qykv28v7879vk1jkjir552xy743agn1jyzsjjbvlawa6hlnpg18";
  };

  pkgs = import nx { config = {}; overlays = []; };
in {
  inherit (pkgs) mellowplayer;
}
