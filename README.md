<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Kalbasit's dotfiles](#kalbasits-dotfiles)
- [Installation](#installation)
  - [First installation](#first-installation)
  - [Subsequent linking](#subsequent-linking)
- [Keeping private things private](#keeping-private-things-private)
- [ZSH personal/work profiles](#zsh-personalwork-profiles)
  - [Profile](#profile)
  - [SSH Agents](#ssh-agents)
- [Author](#author)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Kalbasit's dotfiles

![Screenshot of my shell](http://i.imgur.com/8TnOljy.png)

# Installation

**Warning:** If you want to give these dotfiles a try, you should first
fork this repository, review the code, and remove things you don’t want
or need. Don’t blindly use my settings unless you know what that
entails. Use at your own risk!

You can clone the repository wherever you want, however the path should
not include any spaces. I like to keep it in `~/.dotfiles`. For the rest
of the tutorial, we are going to refer to the clone dotfiles under the
path `~/.dotfiles`.

## First installation

Before installing the dotfiles, you should rename the computer to your
liking. `sed -e 's:crator:mycomputer:g' -i ~/.dotfiles/.osx`

On the first installation, you should run `rake init` which will:

- Initialize OSX, only if ran on OSX. Thanks to [`@mathiasbynens`][6]
  for the idea!
  - Make sure to read over [`.osx`][1] and modify it to your liking.
  - The OSX script will:
    - Install XCode CLI.
    - Install rbenv, installs the latest ruby version and installs
      `bundler` and `git-smart`.
    - Install homebrew, homebrew-bundle and run `brew bundle` which will
      install everything mentioned in [`Brewfile`][2].
    - Run `~/.dotfiles/.private/.osx` if it exists. This allows you to
      add any extra setup steps that are private. I use this file to
      insert Divvy's License and add custom search domains to all
      interfaces.
- Update submodules.
- Switch to ZSH.
- Link everything from `~/.dotfiles` to the home directory.
- Link everything from `~/.dotfiles/.private` **if it exists**, this is
  the perfect place for you to keep private things. See [Keeping things
  private private][3] for more information.
- Install all of Vim plugins, as specified in [`.vimrc`][4]
- Install Go binaries, as specified in [`Rakefile`][5]

## Subsequent linking

When adding a new file to the dotfiles, run `rake` to link any newly
added file.

# Keeping private things private

The dotfiles are not really the place for your private things. But it
does fully support a `.private` sub-folder which you can track in any
way you want. This is folder to keep your `~/.ssh` and your `~/.gnupg`.

![Screenshot of .private](http://i.imgur.com/UvWQmze.png)

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

P.S: the function have above is defined as follows:

```
# have returns 0 if $1 is callable (alias, function or a binary)
# Credit: https://github.com/Daenyth/dotfiles/blob/a22723420e780f04a77ebab8dd2737cfaba43c42/.bashrc#L47
function have() {
  type "$1" &>/dev/null
}
```

## SSH Agents

The ssh agents is managed by [`~/.zsh/functions/ssh-agents`][7] which is loaded on
startup of ZSH. It assigns an agent for each profile, so when you switch
profiles, you will also switch the ssh agent and the keys it manages.

The keys for a particular profile should live under
`~/.ssh/<profile-name>`, so your personal keys (for the default profile)
should live under `~/.ssh/personal` and not `~/.ssh`.

![My ~/.ssh folder](http://i.imgur.com/tNsMlks.png)

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
[7]: https://github.com/kalbasit/dotfiles/blob/master/.zsh/functions/ssh-agents
