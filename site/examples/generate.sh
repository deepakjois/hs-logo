#!/bin/sh

set -e

runlogo() {
  out="svgs/`basename $1 .logo`.svg"
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
  out="svgs/`basename $i .logo`.svg"
  runlogo $i
done
