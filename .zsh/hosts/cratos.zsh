# send_code_apollo sends the code from cratos to apollo
alias send_code_apollo='rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ ~/code/ apollo:/volume1/Code/active/'
# get_code_apollo sends the code from apollo to cratos
alias get_code_apollo='rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ apollo:/volume1/Code/active/ ~/code/'
