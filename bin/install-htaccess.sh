#! /bin/sh

set -x

ncftpput -f $HOME/.ncftp/batdev.cfg / resources/.htaccess
