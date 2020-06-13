#! /bin/sh

set -x
rm resources/grolprep.sqlite
stack exec data-import -- util/resources/new-element1.txt
stack exec data-import -- util/resources/new-element3.txt
stack exec data-import -- util/resources/new-element8.txt
stack exec data-import -- figures
set +x
