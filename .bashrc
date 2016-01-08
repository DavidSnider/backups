source ~/.git-prompt.sh

alias sshu='ssh sniderdj@login.engin.umich.edu'
alias sshy='ssh -Y sniderdj@login.engin.umich.edu'

#restarts touchpad with disabling while typing enabled
alias restart_touchpad='sudo synclient TouchpadOff=1 && sudo synclient TouchpadOff=0 && sudo syndaemon -i 1 -K -d'
alias rt='restart_touchpad'

alias emaca='emacs'
alias emcas='emacs'
alias eamcs='emacs'
alias emacs='emacs -nw'
alias l='ls'
alias mke='make'
alias open='xdg-open'

alias add='git add'
alias pull='git pull'
alias push='git push'
alias commit='git commit -m'
alias st='git st'
alias gr='git gr'
alias br='git br'
alias master='git checkout master'
alias co='git checkout'
alias rebase='git fetch && git rebase origin/master'

alias tl='tmux list-sessions'
alias ta='tmux attach-session -t'

#alias rm='moveToTrash'
moveToTrash() {
    'mv' "$@" '/home/david/.local/share/Trash/files/'
}

alias cd281='cd ~/Documents/281\ IA/Winter16'
alias cd381='cd ~/Documents/Current\ Classes/EECS381'
alias cd398='cd ~/Documents/Current\ Classes/EECS398'
alias cd441='cd ~/Documents/Current\ Classes/EECS441'

alias ssh485='ssh sniderdj@eecs485-09.eecs.umich.edu'
alias sshscheduler='ssh david@159.203.130.220'

alias updatehelper='sudo apt-get dist-upgrade && sudo apt-get autoremove'
alias update='sudo apt-get update && updatehelper'

#alias diff='meld'

alias pack='tar vczf'
alias unpack='tar xzvf'

function prompt {
    local RED="\[\033[0;31m\]"
    local GREEN="\[\033[0;32m\]"
    local BLUE="\[\033[0;34m\]"

    local CYAN="\[\033[0;36m\]"
    local NORMAL="\[\033[0m\]"

    PS1='$(__git_ps1 " (%s)")'
    PS1="$CYAN[\u]$GREEN[\w]$NORMAL$PS1"
    PS1="$PS1\n> "
}
prompt

alias ls='ls --color=auto --group-directories-first'
alias ll='ls -lh'
#set directories to be blue
LS_COLORS='no=00:di=34;01:tw=34;01:ow=34;01'
#set everything else to be white
LS_COLORS=$LS_COLORS':fi=00:ln=00:pi=00:so=00:bd=00:cd=00:or=00:mi=00:ex=00'
#set executables to be red
LS_COLORS=$LS_COLORS':*.sh=31:*.sh=31:*.exe=31:*.bat=31:*.com=31'
export LS_COLORS

export GIT_EDITOR=emacs
export VISUAL=emacs
export EDITOR=emacs

export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/
