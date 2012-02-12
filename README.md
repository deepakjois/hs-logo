Logo interpreter written in Haskell, specialized for turtle graphics. Still very
much a WIP. Lot of the language still needs to be implemented, but it is fairly 
functional already.

For those who like to live dangerously, here is a quickstart:

* Install [diagrams](http://projects.haskell.org/diagrams/) and related dependencies. Here is a [gist of instructions](https://gist.github.com/1683922) if you are on OS X.
* Clone the Git repo for hs-logo
* Run `cabal configure && cabal build`

You can now try out some logo programs in the [examples](https://github.com/deepakjois/hs-logo/tree/master/examples) folder

    % dist/build/hs-logo/hs-logo examples/snowflake.logo -o ~/tmp/snowflake.png

![Snowflake](https://lh3.googleusercontent.com/-UV4m8QM0B3w/TzhPEwWnvwI/AAAAAAAAD7Q/RJE5OOMaU8U/s400/snowflake.png)

    % dist/build/hs-logo/hs-logo examples/design1.logo -o ~/tmp/design1.png

![Design](https://lh4.googleusercontent.com/-nrsWCJizZz4/TzhPGcxKjQI/AAAAAAAAD7Y/NggGkhuPZf0/s400/logo1.png)
