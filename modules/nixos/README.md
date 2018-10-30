# NixOS module

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [`mine.useColemakKeyboardLayout`](#mineusecolemakkeyboardlayout)
- [`mine.home-manager.config`](#minehome-managerconfig)
- [`mine.users`](#mineusers)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

The NixOS module wraps around the upstream NixOS module to provide some
sensible configuration for the NixOS boxes.

## `mine.useColemakKeyboardLayout`

- type: boolean
- default: `false`

When this option is enabled, the keyboard layout is set to Colemak in
early console (i.e initrd), the console and the Xorg server.

## `mine.home-manager.config`

- type: Function
- default: `{ name, uid, isAdmin, nixosConfig }: {...}: {}`

This option is a function that takes `name`, `uid`, `isAdmin` and
`nixosConfig` as parameters and returns a new function that gets set to
`home-manager.users.<name>` to configure the home directory of the user.
See the [home module][1] for more information.

## `mine.users`

- type: attrs of user to `{ uid, isAdmin, home }`.
- default: See [users][2] module.

This option controls the users that get created on the NixOS system.
They automatically inherit the home manager set in`mine.home-manager.config`.

[1]: /modules/home/README.md
[2]: /modules/nixos/general/users.nix
