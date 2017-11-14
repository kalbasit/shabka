# rsync_code sends the code from zeus to apollo
alias rsync_code='rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ ~/code/ apollo:/volume1/Code/active/'
