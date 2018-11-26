<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Modules](#modules)
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
    - [Options](#options-1)
      - [`mine.useColemakKeyboardLayout`](#mineusecolemakkeyboardlayout-1)
      - [`mine.nixosConfig`](#minenixosconfig)
      - [`mine.gnupg.enable`](#minegnupgenable-1)
      - [`mine.batteryNotifier`](#minebatterynotifier)
        - [`mine.batteryNotifier.enable`](#minebatterynotifierenable)
        - [`mine.batteryNotifier.device`](#minebatterynotifierdevice)
        - [`mine.batteryNotifier.notifyCapacity`](#minebatterynotifiernotifycapacity)
        - [`mine.batteryNotifier.hibernateCapacity`](#minebatterynotifierhibernatecapacity)
      - [`mine.git.enable`](#minegitenable)
      - [`mine.less.enable`](#minelessenable)
      - [`mine.neovim.enable`](#mineneovimenable)
      - [`mine.neovim.extraRC`](#mineneovimextrarc)
      - [`mine.neovim.extraKnownPlugins`](#mineneovimextraknownplugins)
      - [`mine.neovim.extraPluginDictionaries`](#mineneovimextraplugindictionaries)
      - [`mine.pet.enable`](#minepetenable)
      - [`mine.taskwarrior.enable`](#minetaskwarriorenable)
      - [`mine.timewarrior.enable`](#minetimewarriorenable)
      - [`mine.theme`](#minetheme)
      - [`mine.workstation.enable`](#mineworkstationenable-1)
        - [`mine.workstation.alacritty.enable`](#mineworkstationalacrittyenable)
        - [`mine.workstation.chromium.enable`](#mineworkstationchromiumenable)
        - [`mine.workstation.dropbox.enable`](#mineworkstationdropboxenable)
        - [`mine.workstation.dunst.enable`](#mineworkstationdunstenable)
        - [`mine.workstation.firefox.enable`](#mineworkstationfirefoxenable)
        - [`mine.workstation.greenclip.enable`](#mineworkstationgreenclipenable)
        - [`mine.workstation.locker.enable`](#mineworkstationlockerenable)
        - [`mine.workstation.mysql-workbench.enable`](#mineworkstationmysql-workbenchenable)
        - [`mine.workstation.rofi.enable`](#mineworkstationrofienable)
        - [`mine.workstation.termite.enable`](#mineworkstationtermiteenable)
      - [`mine.windowManager`](#minewindowmanager)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Modules

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

The NixOS module wraps around the upstream home-manager module to provide some
sensible configuration for all my Unix boxes.

### Options

#### `mine.useColemakKeyboardLayout`

- type: boolean
- default: `false`

When this option is enabled, the keyboard layout is set to Colemak in
early console (i.e initrd), the console and the Xorg server.

#### `mine.nixosConfig`

- type: boolean
- default: `{}`

This option is set by the NixOS module for communicating the system
configuration down to the home-manager configuration.

#### `mine.gnupg.enable`

- type: boolean
- default: false

Enabling this option will enable the gnupg agent and the browser socket.

#### `mine.batteryNotifier`

This module provides a systemd service that notifies the user when the
notifyCapacity is reached and automatically hibernate when the
hibernateCapacity is reached.

##### `mine.batteryNotifier.enable`

- type: boolean
- default: false

Enable this to enable the batteryNotifier.

##### `mine.batteryNotifier.device`

- type: str
- default: "BAT0"

The device representing the battery in your laptop, it must exist under
`/sys/class/power_supply/`.

##### `mine.batteryNotifier.notifyCapacity`

- type: integer
- default: 10

The percentage of the battery when reached a notification will be sent
to the user.

##### `mine.batteryNotifier.hibernateCapacity`

- type: integer
- default: 10

The percentage of the battery when reached, the system will hibernate.

#### `mine.git.enable`

- type: boolean
- default: false

Install and configure git.

#### `mine.less.enable`

- type: boolean
- default: false

Install and configure less.

#### `mine.neovim.enable`

- type: boolean
- default: false

Install and configure neovim.

#### `mine.neovim.extraRC`

- type: str
- default: ""

Extra NeoVim configuration.

#### `mine.neovim.extraKnownPlugins`

- type: attrs
- default: {}

Extra NeoVim known plugins

#### `mine.neovim.extraPluginDictionaries`

- type: list
- default: []

Extra NeoVim plugin dictionary.

#### `mine.pet.enable`

- type: boolean
- default: false

Install and configure neovim.

#### `mine.taskwarrior.enable`

- type: boolean
- default: false

Install and configure taskwarrior.

#### `mine.timewarrior.enable`

- type: boolean
- default: false

Install and configure timewarrior.

#### `mine.theme`

- type: string
- default: "seoul256"

This options controls the theme of all of the applications supported by
the theme. Please see the [theme][16] folder

#### `mine.workstation.enable`

- type: boolean
- default: false

This option is a meta for all the following options, enabling it will
enable them all. Set this to true when setting up a workstation.

##### `mine.workstation.alacritty.enable`

- type: boolean
- default: false

Install and configure alacritty.

##### `mine.workstation.chromium.enable`

- type: boolean
- default: false

Install and configure chromium with all my profiles and generate and
install surfingkeys.

##### `mine.workstation.dropbox.enable`

- type: boolean
- default: false

Install and configure dropbox.

##### `mine.workstation.dunst.enable`

- type: boolean
- default: false

Install and configure dunst.

##### `mine.workstation.firefox.enable`

- type: boolean
- default: false

Install and configure firefox.

##### `mine.workstation.greenclip.enable`

- type: boolean
- default: false

Install and configure greenclip.

##### `mine.workstation.locker.enable`

- type: boolean
- default: false

Install and configure the screen locker.

##### `mine.workstation.mysql-workbench.enable`

- type: boolean
- default: false

Install and configure mysql-workbench.

##### `mine.workstation.rofi.enable`

- type: boolean
- default: false

Install and configure rofi.

##### `mine.workstation.termite.enable`

- type: boolean
- default: false

Install and configure termite.

#### `mine.windowManager`

- type: str
- default: "i3"

Select the window manager, currently supported i3, Plasma5 and Gnome3.

[5]: /modules/nixos/README.md
[6]: /modules/home/README.md
[8]: /modules/home/README.md
[9]: /modules/nixos/general/users.nix
[10]: /modules/nixos/hardware/
[16]: /modules/home/theme
