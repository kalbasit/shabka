# Shabka

Shabka (Arabic for Network) is a declarative congruent representation of
my workstations (desktops, and laptops), network devices and servers.
It's based on the [NixOS][1] operating system, and uses
[home-manager][4] to setup the home directory of the users, for both
NixOS and the other operating systems, including Mac.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation](#documentation)
  - [Shabka structure](#shabka-structure)
  - [NixOS](#nixos)
  - [Home](#home)
- [Author](#author)
- [Credit](#credit)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation

## Shabka structure

| Directory     | Description                                                                                        |
|:------------- |:-------------------------------------------------------------------------------------------------- |
| overlays      | package and module overrides used throughout the configuration.                                    |
| scripts       | various development scripts to help with the development.                                          |
| external      | Nix expressions for fetching externals such as nixos-hardware.                                     |
| hosts         | top-level expressions specific to individual workstations or servers.                              |
| modules/nixos | custom [NixOS][1] modules in the `mine` namespace controlled by host configuration.                |
| modules/home  | custom [home-manager][4] modules in the `mine` namespace controlled by host configuration.         |
| util          | Nix expressions, mainly functions, used as helpers in the actual modules.                          |
| os-specific   | OS-specific configuration files, and bootstrap scripts not belonging to NixOS or the home-manager. |
| libexec       | development helpers, mainly used by the scripts (not invoked directly).                            |
| network       | top-level expressions describing a NixOps network.                                                 |
| terraform     | cloud-resources that are not nixops-able.                                                          |


## NixOS

The NixOS module is documented in details over in the module's [README][5].

## Home

The Home module is documented in details over in the module's [README][6].

# Author

| [![twitter/ylcodes](https://avatars0.githubusercontent.com/u/87115?v=3&s=128)](http://twitter.com/ylcodes "Follow @ylcodes on Twitter") |
|---|
| [Wael Nasreddine](https://github.com/kalbasit) |

# Credit

Thanks to `#nixos` community on Freenode, particularly `infinisil`,
`clever` and `samueldr` for helping me learn how to configure my NixOS
system and debug it.

The host configuration, the modules and the options are inspired by
@dustinlacewell's [dotfiles][7].

# License

All source code is licensed under the [MIT License][3].

[1]: https://nixos.org
[2]: https://nixos.org/nix
[3]: /LICENSE
[4]: https://github.com/rycee/home-manager
[5]: /modules/nixos/README.md
[6]: /modules/home/README.md
[7]: https://github.com/dustinlacewell/dotfiles
