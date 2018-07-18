#!/usr/bin/env bash
set -eEuo pipefail


export INSTANA_LOG_LEVEL_STDOUT=DEBUG
export INSTANA_OVERRIDE_HSLOGGER_ROOT_HANDLER=true

cd `dirname $BASH_SOURCE`/..
stack build
stack exec -- yesod devel

