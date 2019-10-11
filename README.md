<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Shabka](#shabka)
  - [Why Nix?](#why-nix)
  - [Why NixOS?](#why-nixos)
- [Documentation](#documentation)
  - [Shabka structure](#shabka-structure)
- [Author](#author)
- [Credit](#credit)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Shabka

Shabka (Arabic for Network) is a declarative congruent representation of
my workstations (desktops, and laptops), network devices and servers.
It's based on the [NixOS][1] operating system, and uses
[home-manager][4] to setup the home directory of the users, for both
NixOS and the other operating systems, including Mac.

See [What is "declarative congruence"?][5] for more information on the
subject.

## Why Nix?

[Nix][2] is a powerful package manager for Linux and other Unix systems that
makes package management reliable and reproducible. It provides atomic
upgrades and rollbacks, side-by-side installation of multiple versions
of a package, multi-user package management and easy setup of build
environments. [Read more…][12]

Packages are built from Nix expressions, which is a simple functional
language. A Nix expression describes everything that goes into a package
build action (a “derivation”): other packages, sources, the build
script, environment variables for the build script, etc. Nix tries very
hard to ensure that Nix expressions are deterministic: building a Nix
expression twice should yield the same result.

Because it's a functional language, it's easy to support building
variants of a package: turn the Nix expression into a function and call
it any number of times with the appropriate arguments. Due to the
hashing scheme, variants don't conflict with each other in the Nix
store.

Nix has a large set of Nix expressions containing thousands of existing
Unix packages, in the [Nix Packages collection (Nixpkgs)][13].

## Why NixOS?

NixOS is a Linux distribution with a unique approach to package and
configuration management. Built on top of the [Nix package manager][2],
it is completely declarative, makes upgrading systems reliable, and has
[many other advantages][14].

Similar to Nix, NixOS is built upon the concept of atomic system updates
as well as rollbacks, and that includes the kernel and the initrd images
for early boot.

You simply cannot find yourself on a system that does not boot anymore,
provided the hardware is not defective and you have not broken GRUB or
Systemd-boot.

Please see the [NixOS about page][14] for a quick taste of managing a
NixOS system.

# Documentation

Please see [doc/modules](/doc/modules.md) for documentation on all the
supported options.

## Shabka structure

| Directory     | Description                                                                                        |
|:------------- |:-------------------------------------------------------------------------------------------------- |
| overlays      | package and module overrides used throughout the configuration.                                    |
| scripts       | various development scripts to help with the development.                                          |
| external      | Nix expressions for fetching externals such as nixos-hardware.                                     |
| hosts         | top-level expressions specific to individual workstations or servers.                              |
| modules/nixos | custom [NixOS][1] modules under the `shabka` namespace controlled by host configuration.             |
| modules/home  | custom [home-manager][4] modules under the `shabka` namespace controlled by host configuration.      |
| util          | Nix expressions, mainly functions, used as helpers in the actual modules.                          |
| os-specific   | OS-specific configuration files, and bootstrap scripts not belonging to NixOS or the home-manager. |
| libexec       | development helpers, mainly used by the scripts (not invoked directly).                            |
| network       | top-level expressions describing a NixOps network.                                                 |
| terraform     | cloud-resources that are not nixops-able.                                                          |

# Author

| [![twitter/ylcodes](https://avatars0.githubusercontent.com/u/87115?v=3&s=128)](http://twitter.com/ylcodes "Follow @ylcodes on Twitter") | [![risson](https://avatars1.githubusercontent.com/u/18313093?s=128&v=3)](https://risson.space "Browse risson's website") |
|-----------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|
| [Wael Nasreddine](https://github.com/kalbasit)                                                                                          | [Marc 'risson' Schmitt](https://gitlab.com/risson)                                                                                           |

# Credit

Thanks to `#nixos` community on Freenode, particularly `infinisil`,
`clever` and `samueldr` for helping me learn how to configure my NixOS
system and debug it.

The host configuration, the modules and the options are inspired by
[@dustinlacewell][15]'s [dotfiles][7].

The images from the declarative congruence description above are taken
from the paper published on Usenet, written by Steve Traugott and Lance
Brown.

# License

All source code is licensed under the [MIT License][3].

[1]: https://nixos.org
[2]: https://nixos.org/nix
[3]: /LICENSE
[4]: https://github.com/rycee/home-manager
[5]: /doc/congruent.md
[7]: https://github.com/dustinlacewell/dotfiles
[12]: https://nixos.org/nix/about.html
[13]: https://nixos.org/nixpkgs
[14]: https://nixos.org/nixos/about.html
[15]: https://github.com/dustinlacewell
