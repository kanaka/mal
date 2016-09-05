REM               TYPE% -> VALUE%
REM nil           0     -> (unused)
REM false         1     -> (unused)
REM true          2     -> (unused)
REM integer       3     -> int value
REM float         4     -> ???
REM string        5     -> ZS$ index
REM keyword       6     -> ZS$ index
REM symbol        7     -> ZS$ index
REM list next     8     -> ZT% index / or 0
REM                        followed by value unless empty
REM vector next   9     -> ZT% index / or 0
REM                        followed by value unless empty
REM hashmap       12    -> ???
REM mal function  13    -> ???
REM atom          14    -> TYPE% index

INIT_MEMORY:
  REM global error state
  ER%=0
  ER$=""

  REM boxes memory elements
  SZ%=4096
  DIM ZT%(SZ%): REM TYPE ARRAY
  DIM ZV%(SZ%): REM VALUE ARRAY

  REM Predefine nil, false, true
  ZT%(0) = 0
  ZT%(1) = 1
  ZT%(2) = 2
  ZI%=3

  REM string memory
  ZJ%=0
  DIM ZS$(1024)

  REM logic stack
  PT%=-1: REM index of top of PS% stack
  DIM PS%(128): REM stack of ZT% indexes

  REM environment
  REM DIM EKEYS$(1024)
  REM DIM EVALS%(1024)
  RETURN
