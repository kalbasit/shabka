# NixOS module

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

## ``

[1]: /modules/home/README.md
