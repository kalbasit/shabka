<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Darwin](#darwin)
  - [Installation](#installation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Darwin

## Installation

Assuming my system is cloned under
`~/code/personal/base/src/github.com/kalbasit/system` and the private
directories are cloned under `~/private`, from inside the system folder,
please run the following commands:

```shell
$ command -v nix 2>/dev/null || { curl https://nixos.org/nix/install | sh; source ~/.nix-profile/etc/profile.d/nix.sh }
$ command -v brew 2>/dev/null || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
$ brew bundle --file=os-specific/darwin/Brewfile
$ mkdir -p ~/.config/nixpkgs; ln -s `pwd`/hosts/`hostname -s`/home.nix ~/.config/home.nix
$ HM_PATH="$(nix-instantiate --eval --read-write-mode external/home-manager.nix | cut -d\" -f2 | cut -d\" -f1)" nix-shell $HM_PATH -A install
```
