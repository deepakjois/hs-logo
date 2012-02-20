---
title: Installation
---
## Standard installation

Ideally, the only installation step required would be `cabal install
hs-logo`. However, _hs-logo_ depends on version 0.5 of the _diagrams_ package,
which is not released yet. To add additional complexity, the _diagrams-cairo_
package which is used to render the images, depends on a version of _gtkhs_
which is not yet released either. New releases are scheduled to happen soon, and
this will all be resolved in a few days. Till then, you need to follow the steps
below to install the latest and the greatest.

### Install GHC 7.4, cabal and gtk

This is out of scope for this document. On OS X (which I use), you can install
gtk using [homebrew] by following the instructions in this [gist] (ignore instructions about
installing GHC 7.2, which has already been superseded by GHC 7.4).

[gist]: https://gist.github.com/1683922
[homebrew]: http://mxcl.github.com/homebrew/

### Install the latest versions of gtkhs and diagrams

Download and install the latest _gtkhs_

~~~
% darcs get --lazy http://code.haskell.org/gtk2hs/
% cd gtk2hs
% chmod +x bootstrap.sh
% ./bootstrap.sh
~~~

Download and install the latest _diagrams_

~~~
% darcs get --lazy http://patch-tag.com/r/byorgey/active
% cd active && cabal configure && cabal install && cd ..
% darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-core
% cd diagrams-core && cabal configure && cabal install && cd ..
% darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-lib
% cd diagrams-lib && cabal configure && cabal install && cd ..
% darcs get --lazy http://patch-tag.com/r/byorgey/diagrams-cairo
% cd diagrams-cairo && cabal configure && cabal install && cd ..
~~~

### Install hs-logo

~~~
% cabal instal hs-logo
~~~

## Development Setup

If you want to do development on _hs-logo_, I highly recommend using [cabal-dev]
to create a sandbox of dependencies for it. Follow the instructions in the
[bootstrap_dev.sh] file to get setup.

[cabal-dev]: https://github.com/creswick/cabal-dev
[bootstrap_dev.sh]: https://github.com/deepakjois/hs-logo/blob/master/bootstrap_dev.sh

## Reporting Problems

Open an [issue] on github.

[issue]: https://github.com/deepakjois/hs-logo/issues
