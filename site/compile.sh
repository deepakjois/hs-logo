#!/bin/sh
cabal exec ghc -- -Wall -fno-warn-orphans -fno-warn-unused-do-bind  --make Site.hs



