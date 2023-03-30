# Second lua implementation of [mal](https://github.com/kanaka/mal)
Major difference from first lua implementation is tokenization is done manually instead of using regular expressions.

# requirements
* lua 5.3 or greater
* rlwrap (optional) for readline editing [rlwrap](https:
  //github.com/hanslub42/rlwrap)
```console
# for repl with a readline
rlwrap ./stepA_mal.lua
# for repl
./stepA_mal.lua
# for evaluating script file
./stepA_mal.lua <mal-src-file>
```
