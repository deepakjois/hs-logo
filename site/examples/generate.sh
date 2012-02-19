#!/bin/sh

set -e

runlogo() {
  out="images/`basename $1 .logo`.png"
  echo "Running $1"
  ../../dist/build/hs-logo/hs-logo $1 -o $out
}

# Run just once if a filename is passed in
if [ $# = 1 ]
then
  runlogo $1
  exit
fi

for i in `ls sources/*.logo`
do
  out="images/`basename $i .logo`.png"
  runlogo $i
done
