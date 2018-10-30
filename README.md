<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Shabka](#shabka)
  - [What is "declarative congruent" system?](#what-is-declarative-congruent-system)
    - [Divergence](#divergence)
    - [Convergence](#convergence)
    - [Congruence](#congruence)
    - [Conclusion](#conclusion)
  - [Why Nix?](#why-nix)
  - [Why NixOS?](#why-nixos)
- [Documentation](#documentation)
  - [Shabka structure](#shabka-structure)
  - [NixOS](#nixos)
    - [Options](#options)
      - [`mine.useColemakKeyboardLayout`](#mineusecolemakkeyboardlayout)
      - [`mine.home-manager.config`](#minehome-managerconfig)
      - [`mine.users`](#mineusers)
      - [`mine.hardware.intel_backlight.enable`](#minehardwareintel_backlightenable)
      - [`mine.hardware.machine`](#minehardwaremachine)
      - [`mine.serial_console.enable`](#mineserial_consoleenable)
      - [`mine.openvpn.client.expressvpn.enable`](#mineopenvpnclientexpressvpnenable)
      - [`mine.virtualisation.docker`](#minevirtualisationdocker)
      - [`mine.gnupg.enable`](#minegnupgenable)
      - [`mine.plex`](#mineplex)
      - [`mine.plex.enable`](#mineplexenable)
      - [`mine.plex.dataDir `](#mineplexdatadir-)
      - [`mine.printing.enable`](#mineprintingenable)
      - [`mine.tmux.enable`](#minetmuxenable)
      - [`mine.workstation.enable`](#mineworkstationenable)
        - [`mine.workstation.fonts.enable`](#mineworkstationfontsenable)
        - [`mine.workstation.gnome-keyring.enable`](#mineworkstationgnome-keyringenable)
        - [`mine.workstation.networking.enable`](#mineworkstationnetworkingenable)
        - [`mine.workstation.power.enable`](#mineworkstationpowerenable)
        - [`mine.workstation.redshift.enable`](#mineworkstationredshiftenable)
        - [`mine.workstation.sound.enable`](#mineworkstationsoundenable)
        - [`mine.workstation.teamviewer.enable`](#mineworkstationteamviewerenable)
        - [`mine.workstation.virtualbox.enable`](#mineworkstationvirtualboxenable)
        - [`mine.workstation.xorg.enable`](#mineworkstationxorgenable)
  - [Home](#home)
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

## What is "declarative congruent" system?

There are three system management methods that greatly depend on the
tools that you are using for managing these systems

### Divergence

Setting up and updating a system - be it a workstation or a server -
using manual installation, such as setting a new Mac, or by automated
methods such as `cloud-init` for CoreOS, constitutes a divergent system.
It generally implies bad management of a system, as it diverge from the
baseline overtime and is hard to replicate in case of hardware or
software failure.

![Lisa 2002, Figure 1](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/divergence.gif)

### Convergence

Convergence is the process of mutating a divergent live system back to the
baseline by comparing the state of the system to what the state of the
system should look like and performing automated actions to get the
system back to the baseline.

Puppet, Chef, Ansible and other similar tools follow the convergence
method of maintaining systems. These tools are also considered
declarative as they describe exactly how the system should look like.

![Lisa 2002, Figure 2](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/convergence.gif)

### Congruence

Congruence is the practice of maintaining live hosts in complete
compliance with a declarative baseline of the system. In other words, a
congruent system is a system that you cannot alter without altering the
representation in order to make the changes that you are attempting.

![Lisa 2002, Figure 3](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/congruence.gif)

### Conclusion

Working with a declarative baseline for an infrastructure (whatever the
indented use case of the machine or machines in your network) allows the
system to be replicated in a matter of minutes with no manual
configuration or missed steps. It also allows you to know, at any point,
what's the state of your entire infrastructure by simply reading the
declarative baseline of it.

There's an excellent paper by [Steve Traugott and Lance Brown][11] published
in 2002 that goes in details on all three methods.

My hopes with this project is to eliminate the constant need for manual
intervention in order to fix one of my broken machine, for whatever the
reason they broke. Even in the case of hardware failure, all I have to
do is to replace the hardware knowing full well that my entire system
can get back online by simply applying the baseline to a fresh machine
and restore the user data only.

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

The NixOS module wraps around the upstream NixOS module to provide some
sensible configuration for the NixOS boxes.

### Options

#### `mine.useColemakKeyboardLayout`

- type: boolean
- default: `false`

When this option is enabled, the keyboard layout is set to Colemak in
early console (i.e initrd), the console and the Xorg server.

#### `mine.home-manager.config`

- type: Function
- default: `{ name, uid, isAdmin, nixosConfig }: {...}: {}`

This option is a function that takes `name`, `uid`, `isAdmin` and
`nixosConfig` as parameters and returns a new function that gets set to
`home-manager.users.<name>` to configure the home directory of the user.
See the [home module][8] for more information.

#### `mine.users`

- type: attrs of user to `{ uid, isAdmin, home }`.
- default: See [users][9] module.

This option controls the users that get created on the NixOS system.
They automatically inherit the home manager set in`mine.home-manager.config`.

#### `mine.hardware.intel_backlight.enable`

- type: boolean
- default: false

When this option is enabled, The group of the
/sys/class/backlight/*/brightness files is changed to the group `video`
so the user can change the brightness without the need of root.

#### `mine.hardware.machine`

- type: string
- default: empty string

This option describes the make-model, or role of the machine for
automatic hardware setup. See the [hardware][10] folder.

#### `mine.serial_console.enable`

- type: boolean
- default: false

When this option is enabled, the serial console is enabled for the
kernel and for GRUB. Although, GRUB is not automatically enabled,
instead you should do so in the host configuration.

#### `mine.openvpn.client.expressvpn.enable`

- type: boolean
- default: false

Enabling this option will install the ExpressVPN services, but will not
enable any of them.

TODO(low): define the keys and passwords per host.

#### `mine.virtualisation.docker`

- type: boolean
- default: false

Enabling this option will install and enable Docker.

#### `mine.gnupg.enable`

- type: boolean
- default: false

Enabling this option will enable the gnupg agent and the browser socket.

#### `mine.plex`

#### `mine.plex.enable`

- type: boolean
- default: false

Enabling this option will enable the Plex media server.

#### `mine.plex.dataDir `

- type: str
- default: `""`

Set the dataDir of the Plex Media Server

#### `mine.printing.enable`

- type: boolean
- default: false

Enable printing

TODO(low): add an option to configure the drivers

#### `mine.tmux.enable`

- type: boolean
- default: false

Enable TMux program, pre-configured with my preference.

#### `mine.workstation.enable`

- type: boolean
- default: false

This option is a meta for all the following options, enabling it will
enable them all. Set this to true when setting up a workstation.

##### `mine.workstation.fonts.enable`

- type: boolean
- default: false

Enable this option to install `powerline-fonts` and enable fontDir and
ghostscriptFonts.

##### `mine.workstation.gnome-keyring.enable`

- type: boolean
- default: false

Enable this option to start the Gnome3 keyring.

##### `mine.workstation.networking.enable`

- type: boolean
- default: false

Enable this option to start the Network Manager.

##### `mine.workstation.power.enable`

- type: boolean
- default: false

Enable power management.

##### `mine.workstation.redshift.enable`

- type: boolean
- default: false

Enables the redshift service for tunning out blue colors in the dark.

##### `mine.workstation.sound.enable`

- type: boolean
- default: false

Enables sound via Pule audio and installs pavucontrol in the system
packages.

##### `mine.workstation.teamviewer.enable`

- type: boolean
- default: false

Install the TeamViewer application and installs and enables the
TeamViewer service.

##### `mine.workstation.virtualbox.enable`

- type: boolean
- default: false

Install and enable VirtualBox with extension pack.

##### `mine.workstation.xorg.enable`

- type: boolean
- default: false

Enable Xorg and:

- Set the display manager to lightdm
- Enable libinput and naturalScrolling
- Install the Gnome3 and Plasma5 desktop managers

## Home

TODO

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
[8]: /modules/home/README.md
[9]: /modules/nixos/general/users.nix
[10]: /modules/nixos/hardware/
[11]: https://www.usenix.org/legacy/events/lisa2002/tech/full_papers/traugott/traugott.pdf
[12]: https://nixos.org/nix/about.html
[13]: https://nixos.org/nixpkgs
[14]: https://nixos.org/nixos/about.html
