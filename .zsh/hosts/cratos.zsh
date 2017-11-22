# send_code sends the code from cratos to zeus
alias send_code='rsync -avuz --delete --exclude=.snapshots/ ~/code/ zeus:~/code/'
# get_code sends the code from zeus to cratos
alias get_code='rsync -avuz --delete --exclude=.snapshots/ zeus:~/code/ ~/code/'
