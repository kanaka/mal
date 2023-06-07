# Notes
The javascript implementation here is quite old and it will not run on newer versions of nodejs. If you want to use this implementation use an lts nodejs <= to lts/erbium -> v12.22.12.

You can install the above using nvm or any of the node version managers around. Here I assume `nvm`
adjust accordingly.

> Install `nvm` following [these instructions](https://github.com/nvm-sh/nvm#installing-and-updating)

Once that is done, you can get things building issuing from the root folder of your `mal` clone
these commands:
```
$ nvm install lts/erbium
$ make clean^js
$ make test^js
```
