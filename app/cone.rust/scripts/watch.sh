#! /bin/sh

find src/ | entr sh -c 'scripts/build.sh'
