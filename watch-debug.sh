#!/usr/bin/env bash

trap 'exit' ERR

cargo watch -s ./build-debug.sh -d 0.3 --no-restart -i static/\*
