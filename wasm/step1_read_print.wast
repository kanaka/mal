(module $step1_read_print
  (import "env" "memory" (memory $0 256))
  (import "env" "memoryBase" (global $memoryBase i32))

  (func $READ (param $str i32) (result i32)
    (call $read_str (get_local $str)))

  (func $EVAL (param $ast i32) (param $env i32) (result i32)
    (get_local $ast))

  (func $PRINT (param $ast i32) (result i32)
    (call $pr_str (get_local $ast)))

  (func $rep (param $line i32) (param $env i32) (result i32)
    (local $mv1 i32)
    (local $mv2 i32)
    (local $ms i32)
    (block $rep_done
      (set_local $mv1 (call $READ (get_local $line)))
      (if (get_global $error_type) (br $rep_done))

      (set_local $mv2 (call $EVAL (get_local $mv1) (get_local $env)))
      (if (get_global $error_type) (br $rep_done))

;;      (call $PR_MEMORY (i32.const -1) (i32.const -1))
      (set_local $ms (call $PRINT (get_local $mv2)))
    )

;;    (call $PR_MEMORY (i32.const -1) (i32.const -1))
    (call $RELEASE (get_local $mv1))
    (get_local $ms)
  )

  (func $main (result i32)
    ;; Constant location/value definitions
    (local $line i32)
    (local $res i32)

    ;; DEBUG
    (call $printf_1 (STRING "memoryBase: %d\n") (get_global $memoryBase))
    (call $printf_1 (STRING "heap_start: %d\n") (get_global $heap_start))
    (call $printf_1 (STRING "heap_end: %d\n") (get_global $heap_end))
    (call $printf_1 (STRING "mem: %d\n") (get_global $mem))
;;    (call $printf_1 (STRING "string_mem: %d\n") (get_global $string_mem))
    (call $PR_MEMORY (i32.const -1) (i32.const -1))
;;    (call $PR_MEMORY_RAW (get_global $mem)
;;          (i32.add (get_global $mem)
;;                   (i32.mul_u (get_global $mem_unused_start)
;;                              (i32.const 8))))

    ;; Start
    (block $repl_done
      (loop $repl_loop
        (set_local $line (call $readline (STRING "user> ")))
        (if (i32.eqz (get_local $line)) (br $repl_done))
        (if (i32.eq (i32.load8_u (get_local $line)) (i32.const 0))
          (then
            (call $free (get_local $line))
            (br $repl_loop)))
        (set_local $res (call $rep (get_local $line) (i32.const 0)))
        (if (get_global $error_type)
          (then
            (call $printf_1 (STRING "Error: %s\n") (get_global $error_str))
            (set_global $error_type (i32.const 0)))
          (else
            (call $printf_1 (STRING "%s\n") (call $to_String (get_local $res)))))
        (call $RELEASE (get_local $res))
;;        (call $PR_MEMORY (i32.const -1) (i32.const -1))
        (call $free (get_local $line))
        (br $repl_loop)))

    (call $print (STRING "\n"))
    (call $PR_MEMORY (i32.const -1) (i32.const -1))
    (i32.const 0)
  )


  (export "_main" (func $main))
  (export "__post_instantiate" (func $init_memory))
)

