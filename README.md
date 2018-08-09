# kalbasit's System

My system, which describes not only my home dir, but also my entire
machine, is now managed by [Nix][1] for the home
configurations and by [NixOS][2] for the system.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Features](#features)
- [Screenshot](#screenshot)
- [Installation](#installation)
  - [NixOS](#nixos)
- [ZSH personal/work profiles](#zsh-personalwork-profiles)
  - [Profile](#profile)
  - [SSH Agents](#ssh-agents)
- [Author](#author)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


# Features

- [Croscore fonts][8].
- Colemak keyboard layout. My Kinesis Advantage 2 layout and settings are available [here][9].
- i3 window manger (see [i3 README][10] for more information):
  - SourceCodePro font from the [Adobe][8].
  - [seoul256][11] color scheme.
  - uses rofi backed by [i3 dynamic workspaces][12] for workspace
      management. Allows an infinite number of workspaces with Alfred-like
      interface for jumging from one to another.
- Workflow optimised for a storgy-based name-spacing. One i3 workspace,
    with one tmux socket and a dedicated GOPATH. This helps separate
    projects down to the filesystem and allows one git repo (the base) to
    be checked out multiple times with different branches [git help
    worktree][14]. For more information, please checkout my [story based
    workflow manager](https://github.com/kalbasit/swm).
- Vim optimised for working in Go, Python, Ruby, Typescript and many
    more, please see my [Vim README][19] for more information.
- Chromium augmented with Surfingkeys to bring my full Vim experience to
    the browser. With Colemak layout and Github/Travis navigation helper,
    see the [config file][17] for details.

# Screenshot

This is [NixOS][2], running i3 window manager, chromium for browser and
[Alacritty][3] for the terminal. I use `rofi` for i3 workspace
management, and to call binaries and .desktop files.

![Screenshot of my shell](https://i.imgur.com/gNF5iHs.png)

# Installation

## NixOS

```
$ git clone --recurse-submodules https://github.com/kalbasit/system.git
$ cd system
$ sudo ./scripts/bootstrap.sh <machine>
```

Where `<machine>` is a folder that must exist under `machines/<machine>`

# ZSH personal/work profiles

My dotfiles provides support for personal and work profiles. It allows
me to separate personal from work (work being one or more profiles).

The personal profile is loaded by default, it's everything you include
in your regular `~/.dotfiles`.

Profiles live under `~/.zsh/profiles`, it is advised to keep those under
your private repository, `~/.dotfiles/.private/.zsh/profiles` and let
`rake` manage the linking.

## Profile

A profile is defined by two functions:
- `pactivate()` is called when the profile is activated. You can add
  alias, export variables or do anything possible from inside a ZSH
  function.
- `pdeactivate()` is called when the profile is deactivated (or switched
  off). You should basically undo anything done in `pactivate()` above.

Example: `~/.dotfiles/.private/.zsh/profiles/dailymotion.zsh`:

```
function pactivate() {
  alias dev='ssh -A dev'

  if have docker-machine; then
    export DMX_EVE_URL="//${DOCKER_MACHINE_DEV_IP}:1234"
    export DMX_JWT_SECRET="some-long-big-secret"
    export DMX_API_URL="//${DOCKER_MACHINE_DEV_IP}:5678"
  fi

  export AWS_CLI_PROFILE=dmx
  export AWS_PROFILE=dmx
  export DEIS_PROFILE=dmx
  export FLEETCTL_TUNNEL=1.2.3.4
  export DEISCTL_TUNNEL=$FLEETCTL_TUNNEL
}

function pdeactivate() {
  unalias dev 2> /dev/null
  unset DMX_EVE_URL DMX_JWT_SECRET DMX_API_URL \
  FLEETCTL_TUNNEL DEISCTL_TUNNEL AWS_PROFILE DEIS_PROFILE AWS_CLI_PROFILE
}
```

P.S: the function `have` above is defined as follows:

```
# have returns 0 if $1 is callable (alias, function or a binary)
# Credit: https://github.com/Daenyth/dotfiles/blob/a22723420e780f04a77ebab8dd2737cfaba43c42/.bashrc#L47
function have() {
  type "$1" &>/dev/null
}
```

## SSH Agents

The ssh agents is managed by
[`overlays/all/zsh-config/zsh/functions/ssh_agents`][7] which is loaded
on startup of ZSH. It assigns an agent for each profile, so when you
switch profiles, you will also switch the ssh agent and the keys it
manages.

The keys for a particular profile should live under
`~/.ssh/<profile-name>`, so your personal keys (for the default profile)
should live under `~/.ssh/personal` and not `~/.ssh`.

![My ~/.ssh folder](https://i.imgur.com/tNsMlks.png)

# Author

| [![twitter/kalbasit](https://avatars0.githubusercontent.com/u/87115?v=3&s=128)](http://twitter.com/kalbasit "Follow @kalbasit on Twitter") |
|---|
| [Wael Nasreddine](https://github.com/kalbasit) |

# License

All source code is licensed under the [MIT License][4].

[1]: https://nixos.org/nix
[2]: https://nixos.org
[3]: https://github.com/jwilm/alacritty
[4]: https://raw.github.com/kalbasit/system/master/LICENSE
[7]: https://github.com/kalbasit/system/blob/master/overlays/all/zsh-config/zsh/functions/ssh_agents
[8]: https://adobe-fonts.github.io/source-code-pro/
[9]: https://github.com/kalbasit/advantage2
[10]: https://github.com/kalbasit/system/tree/master/.config/i3
[11]: https://github.com/junegunn/seoul256.vim
[12]: https://github.com/kalbasit/system/tree/master/overlays/all/i3-config/bin
[14]: https://git-scm.com/docs/git-worktree
[16]: https://github.com/kalbasit/tmx
[17]: https://github.com/kalbasit/system/blob/master/overlays/all/surfingkeys-config/surfingkeys.js
[19]: https://github.com/kalbasit/system/tree/master/overlays/neovim
