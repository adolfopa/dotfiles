#! /bin/sh

backup_db()
{
    mysqldump -uroot -p$(secret mysql) $1 > ~/$1-backup.sql
}

drop_db()
{
    echo "drop database $1;" | mysql -uroot -p$(secret mysql)
    echo "create database $1;" | mysql -uroot -p$(secret mysql)
}

backup_db "lportal_master"
drop_db "lportal_master"

backup_db "lportal_6_2_x"
drop_db "lportal_6_2_x"

drop_db "lportal_test"
