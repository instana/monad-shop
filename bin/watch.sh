#!/usr/bin/env bash
set -eEuo pipefail

cd `dirname $BASH_SOURCE`/..
find . -iname \*\.hs -o -iname \*\.yaml -o -iname \*\.cabal | entr -r bin/build-and-run.sh

