Logo interpreter written in Haskell, specialized for turtle graphics. Still very
much a WIP. Lot of the language still needs to be implemented, but it is fairly
functional already.

More info at http://deepakjois.github.com/hs-logo

## Quickstart

For those who like to live dangerously, here is a quickstart:

* Install [diagrams](http://projects.haskell.org/diagrams/) and related dependencies. Here is a [gist of instructions](https://gist.github.com/1683922) if you are on OS X.
* Clone the Git repo for hs-logo
* Run `cabal configure && cabal build`

You can now try out some logo programs in the [examples](https://github.com/deepakjois/hs-logo/tree/master/site/examples) folder

    % dist/build/hs-logo/hs-logo site/examples/sources/snowflake.logo -o ~/tmp/snowflake.png

![Snowflake](http://deepakjois.github.com/hs-logo/examples/images/snowflake.png)

    % dist/build/hs-logo/hs-logo site/examples/sources/design1.logo -o ~/tmp/design1.png

![Design](http://deepakjois.github.com/hs-logo/examples/images/design1.png)
