#!/bin/sh
echo url="https://www.duckdns.org/update?domains=bs-home-server&token=81f56645-c14e-407a-97f0-85e3323ab06e&ip=" | curl -k -o ~/duckdns/duck.log -K -
