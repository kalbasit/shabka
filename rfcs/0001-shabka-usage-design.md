---
feature: shabka-usage-design
start-date: 2019-10-14
author: Marc 'risson' Schmitt
co-authors:
  - Wael 'kalbasit' Nasreddine
related-issues:
  - https://github.com/kalbasit/shabka/projects/10
  - https://github.com/kalbasit/shabka/issues/263
  - https://github.com/kalbasit/shabka/issues/246
  - https://github.com/kalbasit/shabka/issues/55
  - https://github.com/kalbasit/shabka/issues/281
---

# Summary
[summary]: #summary

The goal is to design a system where `shabka` is just a tool like
`home-manager`, and relies on configuration files provided by the user.
The tools and ways to configure at the disposition of the user would be those:

* the command `shabka`, installed like `home-manager` which basically does the
  stuff the user wants to do on a regular basis that we currently have in
  `./scripts` ;
* a `dotshabka` like directory for the user's hosts and personal preferences
  settings and default values for `shabka` options, or overriding stuff we
  define in `shabka`. We will refer to this as `dotshabka`.

# Motivation
[motivation]: #motivation

This is important in the process of making `shabka` usable for everyone. A
simple CLI, plus good documentation on how to create configuration files, will
allow the user to quickly get a hang on the tool.

The planned use cases are very simple for now, and can be described as this:

* Usage on a NixOS system, with root access.
  - Management of several hosts' configuration
  - Management of several users per-host.
  - Management of a single users's configuration hosts-wide.
  - Management of a single users' configuration per-host.
* Usage on a Darwin system, with root access.
  - Management of several hosts' configuration
  - Management of several users per-host.
  - Management of a single users's configuration hosts-wide.
  - Management of a single users' configuration per-host.

The expected outcome is a tool well-documented, easy to use, that can fit easily
inside an existing workflow of managing NixOS and home-manager configurations.
The existing configurations would of course need to be tweaked to fit shabka's
workflow.

# Detailed design
[design]: #detailed-design

We'll discuss the three aspects previously introduced in [Summary](#summary).

## `shabka` CLI

The `shabka` CLI will be used by the user to do what they can currently do using
the tools in the `scripts/` directory. This will allow them to do everything
they could do using the `nixos-rebuild` command, plus building a host, checking
what changed in their host's generation between two git refs, and maybe even
pushing their hosts' configurations to Cachix.

Here is what the CLI usage might look like:

### On NixOS hosts: `shabka {switch | boot | test | build | dry-build | dry-activate}`

This will do what `scripts/nixos-rebuild.sh`, or `script/build-host.sh` for
`build shabka`, currently does. Its usage is:

```sh
shabka [-h host] [-r release] {switch | boot | test | build | dry-build | dry-activate} [nixos-rebuild arguments and options]
```

By default the `host` argument is the current hostname, the `release` argument
is the host's release (defined in `dotshabka/hosts/{host}/release`) if
defined, or `shabka`'s default, defined in `shabka/release`.

> This command cannot build a host with a different kernel than the host it is
> being ran on, e.g. a `Darwin` host cannot be built on a `Linux` host.

This command takes care of setting all environment variables and options to
ensure a smooth `nixos-rebuild` depending on what is defined in `shabka`, and
`dotshabka`. The default location of `dotshabka` are defined in their
respective section of this document.

### On Darwin hosts: `shabka {switch | activate | build | check}`

This will do what `scripts/darwin-rebuild.sh` currently does. Its usage is:

```sh
shabka [-h host] [-r release] {switch | activate | build | check} [darwin-rebuild arguments and options]
```

By default the `host` argument is the current hostname, the `release` argument
is the host's release (defined in `dotshabka/hosts/{host}/release`) if
defined, or `shabka`'s default, defined in `shabka/release`.

> This command cannot build a host with a different kernel than the host it is
> being ran on, e.g. a `Darwin` host cannot be built on a `Linux` host.

This command takes care of setting all environment variables and options to
ensure a smooth `darwin-rebuild` depending on what is defined in `shabka`, and
`dotshabka`. The default location of `dotshabka` are defined in their
respective section of this document.

### `shabka diff`

This will do what `scripts/diff-host.sh` used to do. Its usage is:

```sh
shabka diff <host> <base-rev> [target-rev]
```

By default the `target-rev` argument is HEAD.

> This section needs improvement given the fact that we now have 2
> repositories to deal with (`shabka` and `.shabka`, as `dotshabka-user` is
> defined in `.shabka`). This implies a lot of arguments and some designing
> around this command must be done. A future RFC will cover the use cases of
> this command and its design.

### `shabka push-to-cachix`

This will do what `scripts/push-to-cachix.sh` currently does. Its usage is:

```sh
shabka push-to-cachix <cache-name> [host1] [host2] [host3] [...]
```

If no host is given, this command will push every host in `dotshabka/hosts`.
This command requires the environment variable `CACHIX_SIGNING_KEY` to be set.

## `dotshabka` for a user's hosts and personal configuration

This directory will be used to define a user's:

* hosts ;
* personal configuration hosts-wide ;
* personal configuration per-host.

By default, `shabka` will look for this directory in `$HOME/.config/shabka` and
`$HOME/.shabka`, in this order. This should be configurable in some way. Here
is the expected structure of the directory:

* `external/` (optional): Nix expressions for fetching externals such as a
  personal NUR, or a list of your SSH keys.
* `hosts/`: top-level expressions specific to individual hosts. The name of the
  host's folder must match its hostname.

Inside a host's directory, `shabka` will expect the following things:

* `default.nix`: used to import the host's configuration
* `configuration.nix`: The actual host's configuration, where you can use NixOS
  configuration options, and the `nixos` module of `shabka`.
* `home.nix` (optional, imported from `configuration.nix`) : defines a
  function for the user as its now done in the `home.nix` inside a host's
  configuration. In this `home.nix`, the user can then define their personal
  configuration, using the `home` module in `shabka`, overriding it, or directly
  using `home-manager` configuration options.
* `uname`: either containing `Darwin` or `NixOS` (`Linux` is for non-NixOS
  hosts and a later release)
* `release` (optional, defaults to `shabka/release`): defines the release of
  `nixpkgs` you want to use, such as `unstable` or `19.09`.

# Drawbacks
[drawbacks]: #drawbacks

This organization can complicate things for someone discovering `shabka`.
However, having appropriate documentation can ease the first use of `shabka`.

Also, this can complicate the development of `shabka` for us. Having several
repositories might mean that we have to find a way to inform users about changes
upstream. The way `home-manager` does it is with a news system.

# Alternatives
[alternatives]: #alternatives

No alternative has yet been considered.

# Unresolved questions
[unresolved]: #unresolved-questions

The way the `shabka diff` command will work is still a WIP. Making it easy for
the user to use might not be trivial.

Multi-user usage.

# Future work
[future]: #future-work

* Rollbacks
* Non-NixOS systems with nix installed
