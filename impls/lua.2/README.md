# Second Lua implementation of [mal](https://github.com/kanaka/mal)
Major difference from first Lua implementation is tokenisation is done manually instead of using regular expressions.

# requirements
* Lua 5.3 or greater
* rlwrap (optional) for readline editing [rlwrap](https://github.com/hanslub42/rlwrap)
```sh
# for repl with a readline
rlwrap ./stepA_mal.lua
# for repl
./stepA_mal.lua
# for evaluating script file
./stepA_mal.lua <mal-src-file>
```
