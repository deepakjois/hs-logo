#!/bin/sh
for i in `ls sources/*.logo`
do
  out="images/`basename $i .logo`.png"
  echo "Running $i"
  ../dist/build/hs-logo/hs-logo $i -o $out
done
