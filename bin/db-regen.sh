#! /bin/sh

set -x
rm resources/fequiz.sqlite
src/data-import.hs util/resources/new-element1.txt
src/data-import.hs util/resources/new-element3.txt
src/data-import.hs util/resources/new-element8.txt
set +x