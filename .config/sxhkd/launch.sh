#!/usr/bin/env bash

killall sxhkd

echo "---" | tee -a /tmp/sxhkd.log
sxhkd "$@" 2>&1 | tee -a /tmp/sxhkd.log &
disown
echo "SXHKD Launched"
