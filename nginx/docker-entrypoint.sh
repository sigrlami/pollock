#!/bin/sh
set -e

if [ "$1" = 'run' ]; then
  echo "Stating server"
  service nginx start
  tail -f /var/log/nginx/error.log
else
    exec "$@"
fi

