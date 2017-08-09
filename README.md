<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Features](#features)
- [Screenshot](#screenshot)
- [Installation](#installation)
  - [First installation](#first-installation)
    - [OSX](#osx)
  - [Subsequent linking](#subsequent-linking)
- [Keeping private things private](#keeping-private-things-private)
- [ZSH personal/work profiles](#zsh-personalwork-profiles)
  - [Profile](#profile)
  - [SSH Agents](#ssh-agents)
- [Author](#author)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Features

- [Croscore fonts][8].
- Colemak keyboard layout. My Kinesis Advantage 2 layout and settings.
	are available [here][9]
- i3 window manger (see [i3
	README][10] for more information):
	- Cousine font from the [Croscore fonts][8].
	- [seoul256][11] color scheme.
	- uses rofi backed by [i3 dynamic workspaces][12] for workspace
		management. Allows an infinite number of workspaces with Alfred-like
		interface for jumging from one to another. See this demo video
		TODO:[here](#TODO).
	- much more, see the README linked above.
- Workflow optimised for a project-based namespacing. One i3 workspace,
	with one tmux socket and a dedicated GOPATH. This helps separate
	projects down to the filesystem and allows one git repo (the base) to
	be checked out multiple times with different branches [git help
	worktree][14]. For more information, please checkout [my workflow][15]
	and my TMUX session manager [TMX][16]
- Vim optimised for working in Go, Python, Ruby, Typescript and many
	more, please see my [Vim README](#TODO) for more information
- Chromium augmented with Surfingkeys to bring my full Vim experience to
	the browser. With Colemak layout and Github/Travis navigation helper,
	see the [config file][17] for details.

# Screenshot

This is Arch linux, running i3 window manager, chromium for browser and
Termite for the terminal. I use `rofi` for i3 workspace management, and
to call binaries and .desktop files.

![Screenshot of my shell](https://i.imgur.com/gNF5iHs.png)

# Installation

You should clone this repository at `~/.dotfiles`

```
$ git clone https://github.com/kalbasit/dotfiles.git ~/.dotfiles

```

## First installation

On the first installation, you should run `rake init` which will:

- Update submodules.
- Link everything from `~/.dotfiles` to the home directory.
- Link everything from `~/.dotfiles/.private` **if it exists**, this is
  the perfect place for you to keep private things. See [Keeping things
  private private][3] for more information.
- initialize Vim
- Install rbenv, install the latest Ruby and set it as the default.
	Additionally it will install the `bundler` and `git-smart` gems.
- Install some Go binaries defined in the [Rakefile][18] look for
	`GO_BINARIES`.

It will also run the default tasks (that run with simple `rake`):

### OSX

For OSX, run [`.osx`][1] binary. Make sure to read it over and modify it
to your liking, you might want to change the computer host name for
example `sed -e 's:cratos:mycomputer:g' -i .osx`

The OSX script will:
- Install XCode CLI.
- Install homebrew, homebrew-bundle and run `brew bundle` which will
  install everything mentioned in [`Brewfile`][2].
- Run `~/.dotfiles/.private/.osx` if it exists. This allows you to
  add any extra setup steps that are private. I use this file to
  insert Divvy's License and add custom search domains to all
  interfaces.

## Subsequent linking

When adding a new file to the dotfiles, run `rake` to link any newly
added file.

# Keeping private things private

The dotfiles are not really the place for your private things. But it
does fully support a `.private` sub-folder which you can track in any
way you want. This is folder to keep your `~/.ssh` and your `~/.gnupg`.

![Screenshot of .private](https://i.imgur.com/UvWQmze.png)

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

The ssh agents is managed by [`~/.zsh/functions/ssh_agents`][7] which is loaded on
startup of ZSH. It assigns an agent for each profile, so when you switch
profiles, you will also switch the ssh agent and the keys it manages.

The keys for a particular profile should live under
`~/.ssh/<profile-name>`, so your personal keys (for the default profile)
should live under `~/.ssh/personal` and not `~/.ssh`.

![My ~/.ssh folder](https://i.imgur.com/tNsMlks.png)

# Author

| [![twitter/kalbasit](https://avatars0.githubusercontent.com/u/87115?v=3&s=128)](http://twitter.com/kalbasit "Follow @kalbasit on Twitter") |
|---|
| [Wael Nasreddine](https://github.com/kalbasit) |

[1]: https://github.com/kalbasit/dotfiles/blob/master/.osx
[2]: https://github.com/kalbasit/dotfiles/blob/master/Brewfile
[3]: #keeping-private-things-private
[4]: https://github.com/kalbasit/dotfiles/blob/master/.vimrc#L3
[5]: https://github.com/kalbasit/dotfiles/blob/master/Rakefile#L21
[6]: https://github.com/mathiasbynens/dotfiles/blob/master/.osx
[7]: https://github.com/kalbasit/dotfiles/blob/master/.zsh/functions/ssh_agents
[8]: https://en.wikipedia.org/wiki/Croscore_fonts
[9]: https://github.com/kalbasit/advantage2
[10]: https://github.com/kalbasit/dotfiles/tree/master/.config/i3
[11]: https://github.com/junegunn/seoul256.vim
[12]: https://github.com/kalbasit/i3-dynamic-workspaces
[14]: https://git-scm.com/docs/git-worktree
[15]: https://github.com/kalbasit/workflow
[16]: https://github.com/kalbasit/tmx
[17]: https://github.com/kalbasit/dotfiles/blob/master/.surfingkeys.js.erb
[18]: https://github.com/kalbasit/dotfiles/blob/master/Rakefile
