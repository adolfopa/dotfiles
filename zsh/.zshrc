###
### Platform dependent settings
###

if [ -f $HOME/.zsh-machine-settings ]; then
    . $HOME/.zsh-machine-settings
fi

###
### General
###

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="xardon"

COMPLETION_WAITING_DOTS="true"

plugins=(git jump)

source $ZSH/oh-my-zsh.sh

export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/games:$HOME/bin"

# Shortcut for the `jump` plugin
alias j=jump

###
### Emacs
###

## ZSH is running inside `shell-mode`
[[ $EMACS == t ]] && unsetopt zle

## Don't use Emacs when on SSH
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vi'
else
    export EDITOR="$emacsclient"
fi

## Aliases
alias ec="$emacsclient"

export TERM=xterm-256color

###
### Liferay SDK
###

export ANT_OPTS="-Xmx2048m -XX:MaxPermSize=512m"

alias ac="ant compile"
alias acc="ant clean compile"
alias acd="ant clean deploy"
alias acj="ant compile-jsp"
alias adf="ant deploy-fast"
alias afs="ant format-source"

aa()
{
    if cd $(groot) && ant all && cl; then
        osascript -e 'display notification "BUILD SUCCESSFUL" with title "ant all"'
    else
        osascript -e 'display notification "BUILD FAILURE" with title "ant all"'
    fi
}

atc()
{
    if [[ $1 = '-d' ]]; then
	(cd $(groot) && ant test-class -Dtest.class="$2" -Djunit.debug=true && cl)
    else
	(cd $(groot) && ant test-class -Dtest.class="$1" && cl)
    fi
}

gafs()
{
    (cd $(groot) && cd portal-impl && afs)
}

###
### Git
###

alias gau="git add -u"
