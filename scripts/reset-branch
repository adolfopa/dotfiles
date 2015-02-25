#! /bin/sh

die()
{
    if [ -n "$1" ]; then
	echo $1
    fi

    exit 128
}

pull()
{
    git pull --rebase "upstream" $1
}

push()
{
    git push "origin" $1
}

backup_db()
{
    mysqldump -uroot -p$(secret mysql) $1 > ~/$1-backup.sql
}

drop_db()
{
    echo "drop database $1;" | mysql -uroot -p$(secret mysql)
    echo "create database $1;" | mysql -uroot -p$(secret mysql)
}

lookup_root()
{
    while [ $(basename $(pwd)) != 'portal' ]; do
	cd $(dirname $(pwd))
    done

    echo $(pwd)
}

git status > /dev/null || die

git_branch=$(git status -sb | python -c 'import sys; print sys.stdin.readline().split(r"...")[0][3:]')

(pull $git_branch && push $git_branch) || die

if (pwd | grep portal); then
    cd $(lookup_root) && ant all setup-jrebel
else
    echo "skipping ant all, not in portal"
fi
