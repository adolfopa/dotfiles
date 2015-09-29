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

export ANT_OPTS="-Xmx4096m -XX:MaxPermSize=512m"
export GRADLE_OPTS="-Dorg.gradle.daemon=true"

alias ac="ant compile"
alias acc="ant clean compile"
alias acd="ant clean deploy"
alias acj="ant compile-jsp"
alias adf="ant deploy-fast"
alias afs="ant format-source"

alias gw='$(groot)/gradlew'

aa()
{
    if cd $(groot) && ant all && cl; then
        notify "ant all" "BUILD SUCCESSFUL"
    else
        notify "ant all" "BUILD FAILURE"
    fi
}

atc()
{
    local test_class="$1"
    local junit_debug="false"

    if [[ $1 = '-d' ]]; then
        test_class="$2"
        junit_debug="true"
    fi

    (cd $(groot)/portal-impl && \
            ant test-class -Dtest.class="$test_class" -Djunit.debug=$junit_debug && \
            cl)
}

gafs()
{
    (cd $(groot) && cd portal-impl && afs)
}

###
### Git
###

alias gau="git add -u"
