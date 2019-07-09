<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Modules](#modules)
  - [NixOS](#nixos)
    - [Options](#options)
      - [`shabka.useColemakKeyboardLayout`](#mineusecolemakkeyboardlayout)
      - [`shabka.home-manager.config`](#minehome-managerconfig)
      - [`shabka.users`](#mineusers)
      - [`shabka.hardware.intel_backlight.enable`](#minehardwareintel_backlightenable)
      - [`shabka.hardware.machine`](#minehardwaremachine)
      - [`shabka.serial_console.enable`](#mineserial_consoleenable)
      - [`shabka.openvpn.client.expressvpn.enable`](#mineopenvpnclientexpressvpnenable)
      - [`shabka.virtualisation.docker`](#minevirtualisationdocker)
      - [`shabka.gnupg.enable`](#minegnupgenable)
      - [`shabka.plex`](#mineplex)
      - [`shabka.plex.enable`](#mineplexenable)
      - [`shabka.plex.dataDir `](#mineplexdatadir-)
      - [`shabka.printing.enable`](#mineprintingenable)
      - [`shabka.tmux.enable`](#minetmuxenable)
      - [`shabka.workstation.enable`](#mineworkstationenable)
        - [`shabka.workstation.fonts.enable`](#mineworkstationfontsenable)
        - [`shabka.workstation.gnome-keyring.enable`](#mineworkstationgnome-keyringenable)
        - [`shabka.workstation.networking.enable`](#mineworkstationnetworkingenable)
        - [`shabka.workstation.power.enable`](#mineworkstationpowerenable)
        - [`shabka.workstation.redshift.enable`](#mineworkstationredshiftenable)
        - [`shabka.workstation.sound.enable`](#mineworkstationsoundenable)
        - [`shabka.workstation.teamviewer.enable`](#mineworkstationteamviewerenable)
        - [`shabka.workstation.virtualbox.enable`](#mineworkstationvirtualboxenable)
        - [`shabka.workstation.xorg.enable`](#mineworkstationxorgenable)
  - [Home](#home)
    - [Options](#options-1)
      - [`shabka.useColemakKeyboardLayout`](#mineusecolemakkeyboardlayout-1)
      - [`shabka.nixosConfig`](#minenixosconfig)
      - [`shabka.gnupg.enable`](#minegnupgenable-1)
      - [`shabka.batteryNotifier`](#minebatterynotifier)
        - [`shabka.batteryNotifier.enable`](#minebatterynotifierenable)
        - [`shabka.batteryNotifier.device`](#minebatterynotifierdevice)
        - [`shabka.batteryNotifier.notifyCapacity`](#minebatterynotifiernotifycapacity)
        - [`shabka.batteryNotifier.hibernateCapacity`](#minebatterynotifierhibernatecapacity)
      - [`shabka.git.enable`](#minegitenable)
      - [`shabka.less.enable`](#minelessenable)
      - [`shabka.neovim.enable`](#mineneovimenable)
      - [`shabka.neovim.extraRC`](#mineneovimextrarc)
      - [`shabka.neovim.extraKnownPlugins`](#mineneovimextraknownplugins)
      - [`shabka.neovim.extraPluginDictionaries`](#mineneovimextraplugindictionaries)
      - [`shabka.pet.enable`](#minepetenable)
      - [`shabka.taskwarrior.enable`](#minetaskwarriorenable)
      - [`shabka.timewarrior.enable`](#minetimewarriorenable)
      - [`shabka.theme`](#minetheme)
      - [`shabka.workstation.enable`](#mineworkstationenable-1)
        - [`shabka.workstation.alacritty.enable`](#mineworkstationalacrittyenable)
        - [`shabka.workstation.chromium.enable`](#mineworkstationchromiumenable)
        - [`shabka.workstation.dropbox.enable`](#mineworkstationdropboxenable)
        - [`shabka.workstation.dunst.enable`](#mineworkstationdunstenable)
        - [`shabka.workstation.firefox.enable`](#mineworkstationfirefoxenable)
        - [`shabka.workstation.greenclip.enable`](#mineworkstationgreenclipenable)
        - [`shabka.workstation.locker.enable`](#mineworkstationlockerenable)
        - [`shabka.workstation.mysql-workbench.enable`](#mineworkstationmysql-workbenchenable)
        - [`shabka.workstation.rofi.enable`](#mineworkstationrofienable)
        - [`shabka.workstation.termite.enable`](#mineworkstationtermiteenable)
      - [`shabka.windowManager`](#minewindowmanager)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Modules

## NixOS

The NixOS module wraps around the upstream NixOS module to provide some
sensible configuration for the NixOS boxes.

### Options

#### `shabka.useColemakKeyboardLayout`

- type: boolean
- default: `false`

When this option is enabled, the keyboard layout is set to Colemak in
early console (i.e initrd), the console and the Xorg server.

#### `shabka.home-manager.config`

- type: Function
- default: `{ name, uid, isAdmin, nixosConfig }: {...}: {}`

This option is a function that takes `name`, `uid`, `isAdmin` and
`nixosConfig` as parameters and returns a new function that gets set to
`home-manager.users.<name>` to configure the home directory of the user.
See the [home module][8] for more information.

#### `shabka.users`

- type: attrs of user to `{ uid, isAdmin, home }`.
- default: See [users][9] module.

This option controls the users that get created on the NixOS system.
They automatically inherit the home manager set in`shabka.home-manager.config`.

#### `shabka.hardware.intel_backlight.enable`

- type: boolean
- default: false

When this option is enabled, The group of the
/sys/class/backlight/*/brightness files is changed to the group `video`
so the user can change the brightness without the need of root.

#### `shabka.hardware.machine`

- type: string
- default: empty string

This option describes the make-model, or role of the machine for
automatic hardware setup. See the [hardware][10] folder.

#### `shabka.serial_console.enable`

- type: boolean
- default: false

When this option is enabled, the serial console is enabled for the
kernel and for GRUB. Although, GRUB is not automatically enabled,
instead you should do so in the host configuration.

#### `shabka.openvpn.client.expressvpn.enable`

- type: boolean
- default: false

Enabling this option will install the ExpressVPN services, but will not
enable any of them.

TODO(low): define the keys and passwords per host.

#### `shabka.virtualisation.docker`

- type: boolean
- default: false

Enabling this option will install and enable Docker.

#### `shabka.gnupg.enable`

- type: boolean
- default: false

Enabling this option will enable the gnupg agent and the browser socket.

#### `shabka.plex`

#### `shabka.plex.enable`

- type: boolean
- default: false

Enabling this option will enable the Plex media server.

#### `shabka.plex.dataDir `

- type: str
- default: `""`

Set the dataDir of the Plex Media Server

#### `shabka.printing.enable`

- type: boolean
- default: false

Enable printing

TODO(low): add an option to configure the drivers

#### `shabka.tmux.enable`

- type: boolean
- default: false

Enable TMux program, pre-configured with my preference.

#### `shabka.workstation.enable`

- type: boolean
- default: false

This option is a meta for all the following options, enabling it will
enable them all. Set this to true when setting up a workstation.

##### `shabka.workstation.fonts.enable`

- type: boolean
- default: false

Enable this option to install `powerline-fonts` and enable fontDir and
ghostscriptFonts.

##### `shabka.workstation.gnome-keyring.enable`

- type: boolean
- default: false

Enable this option to start the Gnome3 keyring.

##### `shabka.workstation.networking.enable`

- type: boolean
- default: false

Enable this option to start the Network Manager.

##### `shabka.workstation.power.enable`

- type: boolean
- default: false

Enable power management.

##### `shabka.workstation.redshift.enable`

- type: boolean
- default: false

Enables the redshift service for tunning out blue colors in the dark.

##### `shabka.workstation.sound.enable`

- type: boolean
- default: false

Enables sound via Pule audio and installs pavucontrol in the system
packages.

##### `shabka.workstation.teamviewer.enable`

- type: boolean
- default: false

Install the TeamViewer application and installs and enables the
TeamViewer service.

##### `shabka.workstation.virtualbox.enable`

- type: boolean
- default: false

Install and enable VirtualBox with extension pack.

##### `shabka.workstation.xorg.enable`

- type: boolean
- default: false

Enable Xorg and:

- Set the display manager to lightdm
- Enable libinput and naturalScrolling
- Install the Gnome3 and Plasma5 desktop managers

## Home

The NixOS module wraps around the upstream home-manager module to provide some
sensible configuration for all my Unix boxes.

### Options

#### `shabka.useColemakKeyboardLayout`

- type: boolean
- default: `false`

When this option is enabled, the keyboard layout is set to Colemak in
early console (i.e initrd), the console and the Xorg server.

#### `shabka.nixosConfig`

- type: boolean
- default: `{}`

This option is set by the NixOS module for communicating the system
configuration down to the home-manager configuration.

#### `shabka.gnupg.enable`

- type: boolean
- default: false

Enabling this option will enable the gnupg agent and the browser socket.

#### `shabka.batteryNotifier`

This module provides a systemd service that notifies the user when the
notifyCapacity is reached and automatically hibernate when the
hibernateCapacity is reached.

##### `shabka.batteryNotifier.enable`

- type: boolean
- default: false

Enable this to enable the batteryNotifier.

##### `shabka.batteryNotifier.device`

- type: str
- default: "BAT0"

The device representing the battery in your laptop, it must exist under
`/sys/class/power_supply/`.

##### `shabka.batteryNotifier.notifyCapacity`

- type: integer
- default: 10

The percentage of the battery when reached a notification will be sent
to the user.

##### `shabka.batteryNotifier.hibernateCapacity`

- type: integer
- default: 10

The percentage of the battery when reached, the system will hibernate.

#### `shabka.git.enable`

- type: boolean
- default: false

Install and configure git.

#### `shabka.less.enable`

- type: boolean
- default: false

Install and configure less.

#### `shabka.neovim.enable`

- type: boolean
- default: false

Install and configure neovim.

#### `shabka.neovim.extraRC`

- type: str
- default: ""

Extra NeoVim configuration.

#### `shabka.neovim.extraKnownPlugins`

- type: attrs
- default: {}

Extra NeoVim known plugins

#### `shabka.neovim.extraPluginDictionaries`

- type: list
- default: []

Extra NeoVim plugin dictionary.

#### `shabka.pet.enable`

- type: boolean
- default: false

Install and configure neovim.

#### `shabka.taskwarrior.enable`

- type: boolean
- default: false

Install and configure taskwarrior.

#### `shabka.timewarrior.enable`

- type: boolean
- default: false

Install and configure timewarrior.

#### `shabka.theme`

- type: string
- default: "seoul256"

This options controls the theme of all of the applications supported by
the theme. Please see the [theme][16] folder

#### `shabka.workstation.enable`

- type: boolean
- default: false

This option is a meta for all the following options, enabling it will
enable them all. Set this to true when setting up a workstation.

##### `shabka.workstation.alacritty.enable`

- type: boolean
- default: false

Install and configure alacritty.

##### `shabka.workstation.chromium.enable`

- type: boolean
- default: false

Install and configure chromium with all my profiles and generate and
install surfingkeys.

##### `shabka.workstation.dropbox.enable`

- type: boolean
- default: false

Install and configure dropbox.

##### `shabka.workstation.dunst.enable`

- type: boolean
- default: false

Install and configure dunst.

##### `shabka.workstation.firefox.enable`

- type: boolean
- default: false

Install and configure firefox.

##### `shabka.workstation.greenclip.enable`

- type: boolean
- default: false

Install and configure greenclip.

##### `shabka.workstation.locker.enable`

- type: boolean
- default: false

Install and configure the screen locker.

##### `shabka.workstation.mysql-workbench.enable`

- type: boolean
- default: false

Install and configure mysql-workbench.

##### `shabka.workstation.rofi.enable`

- type: boolean
- default: false

Install and configure rofi.

##### `shabka.workstation.termite.enable`

- type: boolean
- default: false

Install and configure termite.

#### `shabka.windowManager`

- type: str
- default: "i3"

Select the window manager, currently supported i3, Plasma5 and Gnome3.

[5]: /modules/nixos/README.md
[6]: /modules/home/README.md
[8]: /modules/home/README.md
[9]: /modules/nixos/general/users.nix
[10]: /modules/nixos/hardware/
[16]: /modules/home/theme
