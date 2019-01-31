#!/usr/bin/env bash

set -euo pipefail

# TODO: This must be able to mount any host by parsing directly the hardware
# configuration

# NOTE: We allow swap to error out
set +e
swapon /dev/mapper/cryptswap
set +e

mount -o subvol=@nixos/@root /dev/mapper/cryptroot /mnt
mount -o subvol=@nixos/@home /dev/mapper/cryptroot /mnt/home
mount -o subvol=@yl /dev/mapper/cryptroot /mnt/yl
mount -o subvol=@yl/code /dev/mapper/cryptroot /mnt/yl/code
mount -o subvol=@yl/private /dev/mapper/cryptroot /mnt/yl/private
