# rsync_code sends the code from cratos to zeus
alias rsync_code='rsync -avuz --delete --exclude=.snapshots/ ~/code/ zeus:~/code/'
