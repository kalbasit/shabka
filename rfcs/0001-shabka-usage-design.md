---
feature: shabka-usage-design
start-date: 2019-10-14
author: Marc 'risson' Schmitt
co-authors: Wael 'kalbasit' Nasreddine
related-issues: https://github.com/kalbasit/shabka/projects/10 https://github.com/kalbasit/shabka/issues/263 https://github.com/kalbasit/shabka/issues/246 https://github.com/kalbasit/shabka/issues/55 https://github.com/kalbasit/shabka/issues/281
---

# Summary
[summary]: #summary

The goal is to design a system where `shabka` is just a tool like
`home-manager`, and relies on configuration files provided by the user.
The tools and ways to configure at the disposition of the user would be those:

* the command `shabka`, installed like `home-manager` which basically does the
  stuff the user wants to do on a regular basis that we currently have in
  `./scripts` ;
* a `dotshabka` like directory for the user's personal preferences settings and
  default values for `shabka` options, or overriding stuff we define in
  `shabka`. We will refer to this as `.shabka` ;
* a `dotshabka` like directory for the user's hosts. We will refer to this as
  `.hosts`.

# Motivation
[motivation]: #motivation

This is important in the process of making `shabka` usable for everyone. A
simple CLI, plus good documentation on how to create configuration files, will
allow the user to quickly get a hang on the tool.

The planned use cases are very simple for now, and can be described as this:

* Usage on a NixOS system, with root access.
* Management of several hosts' configuration
* Management of several users per-host.
* Management of several users' configuration per-host.

The expected outcome is a tool well-documented, easy to use, that can fit easily
inside an existing workflow of managing NixOS and home-manager configurations.

# Detailed design
[design]: #detailed-design

This is the bulk of the RFC. Explain the design in enough detail for somebody
familiar with the ecosystem to understand, and implement.  This should get
into specifics and corner-cases, and include examples of how the feature is
used.

We'll discuss the three aspects previously introduced in [Summary](#summary).

## `shabka` CLI

The `shabka` CLI will be used by the user to do what he can currently do using
the tools is the `scripts/` directory. This will allow him to do everything he
could do using the `nixos-rebuild` command, plus building a host, checking what
changed in his host's generation between two git refs, and maybe even pushing
his hosts' configurations to Cachix.

Here is what the CLI usage might look like:

### `shabka rebuild`

This will do what `scripts/nixos-rebuild.sh` currently does. Its usage is:

```sh
shabka rebuild [-h host] [-r release] {nixos-rebuild arguments and options}
```

By default the `host` argument is the current hostname, the `release` argument
is the host's release (defined in `.hosts/hosts/{host}/.release`) if
defined, or `shabka`'s default, defined in `shabka/.release`.

This command takes care of setting all environment variables and options to
ensure a smooth `nixos-rebuild` depending on what is defined in `shabka`,
`.hosts` and `.shabka`. The default locations of `.hosts` and `.shabka` are
defined in their respective section of this document.

### `shabka build`

This will do what `scripts/build-host.sh` currently does. Its usage is:

```sh
shabka build [-r release] [host]
```

By default the `host` argument is the current hostname, the `release` argument
is the host's release (defined in `.hosts/hosts/{host}/.release`) if
defined, or `shabka`'s default, defined in `shabka/.release`.

> This command cannot build a host with a different kernel than the host it is
> being ran on, e.g. a `Darwin` host cannot be built on a `Linux` host.

This command takes care of setting all environment variables and options to
ensure a smooth `nix-build` depending on what is defined in `shabka`, `.hosts`
and `.shabka`. The default locations of `.hosts` and `.shabka` are defined in
their respective section of this document.

### `shabka diff`

This will do what `scripts/diff-host.sh` used to do. Its usage is:

```sh
shabka diff <host> <base-rev> [target-rev]
```

By default the `target-rev` argument is HEAD.

/!\ This section needs improvement given the fact that we now have 3
repositories to deal with. This implies a lot of arguments and some designing
around this command must be done.

### `shabka push-to-cachix`

This will do what `scripts/push-to-cachix.sh` currently does. Its usage is:

```sh
shabka push-to-cachix <cache-name> [host1] [host2] [host3] [...]
```

If no host is given, this command will push every host in `.hosts/hosts`. This
command requires `CACHIX_SIGNING_KEY` to be set.

## `.shabka` for a user configuration

This directory will be used to define a user's personal configuration.

This directory must have a `home.nix` that defines a
function for the user as its now done in the `home.nix` inside a host's
configuration. In this `home.nix`, the user can then define its personal
configuration, using the `home` module in `shabka`, overriding it, or directly
using `home-manager` configuration options.

This `home.nix` will later be imported inside a host's configuration, when
defining `shabka.users.users`, by assigning one function per user. This allows
for the user of the same host to have different configurations. Also, it makes
it easy for a user's configuration to be imported in several `.hosts`, and even
be used outside of the `shabka` use case.

## `.hosts` for hosts configuration

By default, `shabka` will look for this directory in `~/.config/hosts` and
`~/.hosts`, in this order. This should be configurable in some way. Here is the
expected structure of the directory:

* `external/`: Nix expressions for fetching externals such as a personal NUR,
  or a list of your SSH keys.
* `hosts/`: top-level expressions specific to individual hosts. The name of the
  host's folder must match its hostname.

Inside a host's directory, `shabka` will expect the following things:

* `.uname`: either containing `Darwin` or `Linux`
* `default.nix`
* `configuration.nix`: The actual host's configuration, where you can use NixOS
  configuration options, and the `nixos` module of `shabka`.

In addition, you can also define the release of `nixpkgs` you want to use
inside a `.release` file, such as `unstable` or `19.09`.

# Drawbacks
[drawbacks]: #drawbacks

Why should we *not* do this?

# Alternatives
[alternatives]: #alternatives

What other designs have been considered? What is the impact of not doing this?

# Unresolved questions
[unresolved]: #unresolved-questions

What parts of the design are still TBD or unknowns?

# Future work
[future]: #future-work

* Rollbacks
* non-nixos systems with nix installed

What future work, if any, would be implied or impacted by this feature
without being directly part of the work?
