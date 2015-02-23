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

export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$HOME/bin"

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

export ANT_OPTS="-Xmx1024m -XX:MaxPermSize=512m"

alias aa="cd $(groot) && ant all && cl"
alias ac="ant compile"
alias acc="ant clean compile"
alias acd="ant clean deploy"
alias acj="ant compile-jsp"
alias adf="ant deploy-fast"
alias afs="ant format-source"

atc()
{
    if [[ $1 = '-d' ]]; then
	ant test-class -Dtest.class="$2" -Djunit.debug=true
    else
	ant test-class -Dtest.class="$1"
    fi

    cl
}

###
### Git
###

alias gau="git add -u"
