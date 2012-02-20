#!/bin/sh

# * Install GHC 7.4, cabal and GTK related dependencies
# * Install cabal-dev: `cabal install cabal-dev`
# * Download sources for all major hs-logo dependencies in parent folder:
#   - darcs get --lazy http://code.haskell.org/gtk2hs/
#   - darcs get --lazy http://patch-tag.com/r/byorgey/active
#   - darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-core
#   - darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-lib
#   - darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-cairo
# * Run `cabal configure && cabal build` in each of the subfolders under gtk2hs: tools, glib, gio, cairo, pango, gtk
# * Run `cabal configure && cabal build` in each of the other packages above
# * Run this script
# * If everything went well, you have a sandboxed development environment with all your dependencies

cabal-dev add-source ../diagrams-core ../active ../diagrams-lib ../gtk2hs/tools ../gtk2hs/glib ../gtk2hs/gio ../gtk2hs/cairo ../gtk2hs/pango ../gtk2hs/gtk ../diagrams-cairo
cabal-dev install
