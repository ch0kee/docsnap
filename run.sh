#!/bin/bash

# Kill stuck process
killall DocSnap

# Clear logs
rm log/*.log

# Launch server
~/.cabal/bin/DocSnap

echo ">> terminated properly <<"
