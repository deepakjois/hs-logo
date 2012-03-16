---
title: Installation
---
## Installation

### Install GHC 7.4 and cabal

This is out of scope for this document. On OS X (which I use), you can install
Haskell Platform using [homebrew]. I haven't tried it, but this might work for you.

[homebrew]: http://mxcl.github.com/homebrew/

~~~
% brew install haskell-platform --devel --64bit
~~~

### Install hs-logo

~~~
% cabal install hs-logo
~~~

This will install `hs-logo` and related dependencies like [diagrams] and the [diagrams-svg] backend.

[diagrams]: http://projects.haskell.org/diagrams/
[diagrams-svg]: http://hackage.haskell.org/package/diagrams-svg

## Reporting Problems

Open an [issue] on github.

[issue]: https://github.com/deepakjois/hs-logo/issues
