#!/usr/bin/env bash

set -euo pipefail

# TODO: This must be able to mount any host by parsing directly the hardware
# configuration

swapon /dev/mapper/cryptswap

mount -o subvol=@nixos/@root /mnt
mount -o subvol=@nixos/@home /mnt/home
mount -o subvol=@yl /mnt/yl
mount -o subvol=@yl/code /mnt/yl/code
mount -o subvol=@yl/private /mnt/yl/private
