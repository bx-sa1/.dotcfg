#!/bin/sh

cmd="beet -c beet_config.yaml import -A --move"
pkexec --user navidrome $cmd "$1" 
