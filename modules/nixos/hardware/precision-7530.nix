{ config, lib, ... }:

with lib;

let

  pinnedNH = import ../../../external/nixos-hardware.nix;

in {
  # config = mkIf (config.mine.hardware.machine == "precision-7530") {
    imports = [
      "${pinnedNH}/common/cpu/intel"
      "${pinnedNH}/common/pc/laptop"
      "${pinnedNH}/common/pc/laptop/ssd"
    ];
  # };
}
