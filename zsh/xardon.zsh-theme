function z_git_branch_name()
{
    local name=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)

    if [[ ! -z $name ]]; then
        local color="yellow"

        if [[ $(git status --porcelain -uno | wc -l) -gt 0 ]]; then
            color="red"
        fi

        echo -n "%{$fg[$color]%}$name "
    fi
}

local ret_status="%(?:%{$fg_bold[green]%}:%{$fg_bold[red]%}%s)"

PROMPT='${ret_status}%3~ $(z_git_branch_name)%b%f\$ '
