#!/usr/bin/env bash

export nixpkgs=/code/personal/base/src/github.com/kalbasit/system/external/nixpkgs:nixos-config=/code/personal/base/src/github.com/kalbasit/system/machines/hades/configuration.nix:nixos-hardware=/code/personal/base/src/github.com/kalbasit/system/external/nixos-hardware

nixos-rebuild switch
