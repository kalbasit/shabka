dotfiles
========

My dotfiles

To decrypt encrypted files, install [git-crypt](https://github.com/AGWA/git-crypt)

# Create your own

Start with a freshly installed Mac and follow the following commands

Install homebrew and brew bundle

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap Homebrew/bundle
```

Clone my dotfiles and clean it up from my own encrypted files Do not
forget to replace the URL with your own fork.

```
git clone --recursive git@github.com:kalbasit/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
brew bundle
git-crypt status | grep -v 'not encrypted' | awk '{print $2}' | xargs git rm
git rm -rf .git-crypt
git commit -m "forking the repo, remove kalbasit's encrypted files"
git-crypt init
echo "`dd if=/dev/urandom of=/dev/stdout bs=1024 count=1`OK`dd if=/dev/urandom of=/dev/stdout bs=1024 count=1`" > .encrypted
cp .gitconfig.unsecure .gitconfig
git add .
git commit -m "add my stuff"
rake
```

Now you should import your GnuPG stuff into `~/.dotfiles/.gnupg` or
generate a new one if you do not have one. Once you have it all
generated you can go ahead and add your GPG key in the repo so git-crypt
can decrypt the next time you clone!

```
git-crypt add-gpg-user <ID or Email>
```

Now add everything and commit. GnuPG stuff should be encrypted by
default. See `~/.dotfiles/.gitattributes` for the list of encrypted
files.
