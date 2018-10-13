(module $step0_repl
  (import "env" "memory" (memory $0 256))
  (import "env" "memoryBase" (global $memoryBase i32))

  (func $READ (param $str i32) (result i32)
    (get_local $str))

  (func $EVAL (param $ast i32) (param $env i32) (result i32)
    (get_local $ast))

  (func $PRINT (param $ast i32) (result i32)
    (get_local $ast))

  (func $rep (param $str i32) (result i32)
    (call $PRINT
          (call $EVAL
                (call $READ (get_local $str))
                (i32.const 0))))

  (func $main (result i32)
    ;; Constant location/value definitions
    (local $line i32)

    ;; Start
    (block $repl_done
      (loop $repl_loop
        (set_local $line (call $readline (STRING "user> ")))
        (if (i32.eqz (get_local $line)) (br $repl_done))
        (call $printf_1 (STRING "%s\n") (call $rep (get_local $line)))
        (call $free (get_local $line))
        (br $repl_loop)))

    (call $print (STRING "\n"))
    (i32.const 0)
  )


  (export "_main" (func $main))
  (export "__post_instantiate" (func $init_sprintf_mem))
)

