# Kalbasit's dotfiles

![Screenshot of my shell](http://i.imgur.com/8TnOljy.png)

## Installation

**Warning:** If you want to give these dotfiles a try, you should first
fork this repository, review the code, and remove things you don’t want
or need. Don’t blindly use my settings unless you know what that
entails. Use at your own risk!

You can clone the repository wherever you want, however the path should
not include any spaces. I like to keep it in `~/.dotfiles`. For the rest
of the tutorial, we are going to refer to the clone dotfiles under the
path `~/.dotfiles`.

### First installation

On the first installation, you should run `rake init` which will:

- Initialize OSX, only if ran on OSX. Thanks to [`@mathiasbynens`][6]
  for the idea!
  - Make sure to read over [`.osx`][1] and modify it to your liking.
  - The OSX script will:
    - Install XCode CLI.
    - Install rbenv, installs the latest ruby version and installs
      `bundler` and `git-smart`.
    - Install homebrew.
    - Install homebrew bundle.
    - Run `brew bundle` which will install everything mentioned in
      [`Brewfile`][2].
- Update submodules.
- Switch to ZSH.
- Link everything from `~/.dotfiles` to the home directory.
- Link everything from `~/.dotfiles/.private` **if it exists**, this is
  the perfect place for you to keep private things. See [Keeping things
  private private][3] for more information.
- Install all of Vim plugins, as specified in [`.vimrc`][4]
- Install Go binaries, as specified in [`Rakefile`][5]

### Subsequent linling

When adding a new file to the dotfiles, run `rake` to link any newly
added file.

# Keeping private things private

The dotfiles are not really the place for your private things. But it
does fully support a `.private` sub-folder which you can track in any
way you want. This is folder to keep your `~/.ssh` and your `~/.gnupg`.

![Screenshot of .private](http://i.imgur.com/UvWQmze.png)

# ZSH personal/work profiles

My dotfiles provides support for personal and work profiles.

# SSH Agent

TBD

[1]: https://github.com/kalbasit/dotfiles/blob/master/.osx
[2]: https://github.com/kalbasit/dotfiles/blob/master/Brewfile
[3]: #keeping-private-things-private
[4]: https://github.com/kalbasit/dotfiles/blob/master/.vimrc#L3
[5]: https://github.com/kalbasit/dotfiles/blob/master/Rakefile#L21
[6]: https://github.com/mathiasbynens/dotfiles/blob/master/.osx
