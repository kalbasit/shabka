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
