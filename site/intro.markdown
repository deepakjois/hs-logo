_hs-logo_ is an interpreter for the [Logo Programming Language][logo], written
in [Haskell]. It is specialised for Turtle graphics, and is not intended to be a
full-fledged Logo interpreter.

[logo]: http://www.cs.berkeley.edu/~bh/logo.html
[Haskell]: http://haskell.org

You can get the latest version from hackage using `cabal install hs-logo`, as
long as you have Haskell installed. Browse the [examples] to get started. You
can execute your logo files to generate an image like this.

[examples]: examples.html

~~~
% hs-logo example.logo -o example.png
~~~

The interpreter is still a work in progress, and more primitives will be added
soon. I also plan to write user guide sometime before the next release. You can
find the sources at the [hs-logo project on github][github].

[github]: http://github.com/deepakjois/hs-logo

