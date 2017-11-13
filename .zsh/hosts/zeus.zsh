# rsync_code sends the code from zeus to apollo
alias rsync_code='rsync -avuz --delete --exclude=.snapshots/ --exclude=pkg/ ~/code/ apollo:/volume1/Code/active/'
