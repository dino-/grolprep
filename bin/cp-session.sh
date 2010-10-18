#! /bin/sh


function usage {
   basename=$(basename $0)
   cat <<USAGE
ERROR: missing arguments

$basename - Migrate grolprep session data from one installation to another

usage: $basename SRCDIR DESTDIR

Specify the dirs directly above session/ :

   /var/www/grolprep-1.0.1.5/share/grolprep-1.0.1.5/session
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  This part for SRC

   /var/www/grolprep/share/grolprep-1.0.1.6/session
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^          This part for DEST
USAGE

   exit 1
}


srcPrefix="$1"
destPrefix="$2"

[ -z "$srcPrefix" ] && usage
[ -z "$destPrefix" ] && usage

destSession="$destPrefix/session"

set -x

[ -x $destSession ] || mkdir $destSession

cp $srcPrefix/session $destSession

chown -R www-data:www-data $destSession
