# bash aliases
# Peter Sujan

# environment variables
export robocop='robocop.banatao.berkeley.edu'
export EDITOR='emacs -nw'
export LANG=en_US.UTF-8

# logins for various class servers
#export INFOKEY='~/.ssh/Info290T.pem'
export INFOUSER='ubuntu'
export INFOSERVER='54.200.79.119'
export INFOLOGIN=$INFOUSER@INFOSERVER

# a lot of the following is defunct
export BERKELEYUID=945621
export STATSERVER=98.143.35.205
export S135LOGIN=s135-$BERKELEYUID@$STATSERVER
export ISCHOOL='peterasujan@ischool.berkeley.edu'

# assorted
alias term='gnome-terminal'
alias temp-monitor='watch -n1 acpi -t'

# makes emacs default to full screen
alias emacs='emacs -mm'

# ssh related
alias ssh-stat='ssh $S135LOGIN'
#alias sshischool='ssh $ISCHOOL' # defunct, I believe
alias sshcop='ssh $robocop'
alias sshinfo='ssh -i ~/.ssh/Info290T.pem $INFOUSER@$INFOSERVER'