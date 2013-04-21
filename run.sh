#!/bin/bash

# Kill stuck process
killall DocSnap

# Clear logs
rm log/*.log

# Launch server
DocSnap

echo ">> terminated properly <<"
