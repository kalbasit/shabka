#!/usr/bin/env bash

set -euo pipefail

# TODO: This must be able to mount any host by parsing directly the hardware
# configuration
# nix eval "(import ./hosts/hades/hardware-configuration.nix {lib = (import <nixpkgs> {}).lib;}).fileSystems"

# NOTE: We allow swap to error out
set +e
swapon /dev/mapper/cryptswap
set +e

mount -o subvol=@nixos/@root /dev/mapper/cryptroot /mnt
mount -o subvol=@nixos/@home /dev/mapper/cryptroot /mnt/home
mount -o subvol=@yl /dev/mapper/cryptroot /mnt/yl
mount -o subvol=@code /dev/mapper/cryptroot /mnt/yl/code
mount -o subvol=@private /dev/mapper/cryptroot /mnt/yl/private

mount /dev/nvme0n1p1 /mnt/boot
