#!/bin/bash
set -e

if [ "$1" = 'run' ]; then
    sleep 2
    stack exec -- pollock -p 4100 
else
    exec "$@"
fi
