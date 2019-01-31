#!/usr/bin/env bash

set -euo pipefail

# TODO: This must be able to mount any host by parsing directly the hardware
# configuration

echo ">>> Starting, please expect to enter your cryptsetup password!"

cryptsetup luksOpen /dev/nvme0n1p2 cryptkey
cryptsetup luksOpen --key-file=/dev/mapper/cryptkey /dev/nvme0n1p3 cryptswap
cryptsetup luksOpen --key-file=/dev/mapper/cryptkey /dev/nvme0n1p4 cryptroot

swapon /dev/mapper/cryptswap

mount -o subvol=@nixos/@root /mnt
mount -o subvol=@nixos/@home /mnt/home
mount -o subvol=@yl /mnt/yl
mount -o subvol=@yl/code /mnt/yl/code
mount -o subvol=@yl/private /mnt/yl/private
