#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
echo WATCH DOES NOT WORK! -- Use bin/build-and-run.sh
exit 1
find . -iname \*\.hs -o -iname \*\.yaml -o -iname \*\.cabal | entr -r bin/build-and-run.sh

