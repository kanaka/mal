(module $step1_read_print
  (import "env" "memory" (memory $0 256))
  (import "env" "memoryBase" (global $memoryBase i32))

  ;; READ
  (func $READ (param $str i32) (result i32)
    (call $read_str (get_local $str))
  )

  ;; EVAL
  (func $EVAL_AST (param $ast i32) (param $env i32) (result i32)
    (local $res i32)
    (local $val2 i32)
    (local $val3 i32)
    (local $ret i32)
    (local $empty i32)
    (local $current i32)
    (local $type i32)
    (local $res2 i64)
    (local $found i32)

    (if (get_global $error_type) (return (i32.const 0)))
    (set_local $type (call $TYPE (get_local $ast)))

    ;;; switch(type)
    (block $done
      (block $default (block (block
      (br_table 2 2 2 2 2 0 1 1 1 2 2 2 2 2 2 2 (get_local $type)))
      ;; symbol
      ;; found/res returned as hi 32/lo 32 of i64
      (set_local $res2 (call $HASHMAP_GET (get_local $env) (get_local $ast)))
      (set_local $res (i32.wrap/i64 (get_local $res2)))
      (set_local $found (i32.wrap/i64 (i64.shr_u (get_local $res2)
                                                 (i64.const 32))))
      (if (i32.eqz (get_local $found))
          (call $THROW_STR_1 (STRING "'%s' not found")
                (call $to_String (get_local $ast))))
      (set_local $res (call $INC_REF (get_local $res)))

      (br $done))
      ;; list, vector, hashmap
      ;; MAP_LOOP_START
      (set_local $res (call $MAP_LOOP_START (get_local $type)))
      ;; push MAP_LOOP stack
      ;;; empty = current = ret = res
      (set_local $ret (get_local $res))
      (set_local $current (get_local $res))
      (set_local $empty (get_local $res))

      (block $done
        (loop $loop
          ;; check if we are done evaluating the source sequence
          (if (i32.eq (call $VAL0 (get_local $ast)) (i32.const 0))
            (br $done))

          (if (i32.eq (get_local $type) (get_global $HASHMAP_T))
            (then
              (set_local $res (call $EVAL (call $MEM_VAL2_ptr (get_local $ast))
                                                (get_local $env))))
            (else
              (set_local $res (call $EVAL (call $MEM_VAL1_ptr (get_local $ast))
                                                (get_local $env)))))
          (set_local $val2 (get_local $res))

          ;; if error, release the unattached element
          (if (get_global $error_type)
            (then
              (call $RELEASE (get_local $res))
              (set_local $res (i32.const 0))
              (br $done)))

          ;; for hash-maps, copy the key (inc ref since we are going
          ;; to release it below)
          (if (i32.eq (get_local $type) (get_global $HASHMAP_T))
            (then
              (set_local $val3 (get_local $val2))
              (set_local $val2 (call $MEM_VAL1_ptr (get_local $ast)))
              (drop (call $INC_REF (get_local $ast)))))

          ;; MAP_LOOP_UPDATE
          (set_local $res (call $MAP_LOOP_UPDATE (get_local $type)
                                (get_local $empty) (get_local $current)
                                (get_local $val2) (get_local $val3)))
          (if (i32.le_u (get_local $current) (get_global $EMPTY_HASHMAP))
            ;; if first element, set return to new element
            (set_local $ret (get_local $res)))
          ;; update current to point to new element
          (set_local $current (get_local $res))

          (set_local $ast (call $MEM_VAL0_ptr (get_local $ast)))
  
          (br $loop)
        )
      )
      ;; MAP_LOOP_DONE
      (set_local $res (get_local $ret))
      ;; EVAL_AST_RETURN: nothing to do
      (br $done))
      ;; default
      (set_local $res (call $INC_REF (get_local $ast)))
    )

    (get_local $res)
  )

  (type $fnT (func (param i32) (result i32)))

  (table anyfunc
    (elem
      $add $subtract $multiply $divide))

  (func $EVAL (param $ast i32) (param $env i32) (result i32)
    (local $res i32)
    (local $f_args i32)
    (local $f i32)
    (local $args i32)
    (local $type i32)
    (local $ftype i32)

    (set_local $res (i32.const 0))
    (set_local $f_args (i32.const 0))
    (set_local $f (i32.const 0))
    (set_local $args (i32.const 0))
    (set_local $type (call $TYPE (get_local $ast)))

    (if (get_global $error_type) (return (i32.const 0)))

    (if (i32.ne (get_local $type) (get_global $LIST_T))
      (return (call $EVAL_AST (get_local $ast) (get_local $env))))

    ;; APPLY_LIST
    (if (call $EMPTY_Q (get_local $ast))
      (return (call $INC_REF (get_local $ast))))

    ;; EVAL_INVOKE
    (set_local $res (call $EVAL_AST (get_local $ast) (get_local $env)))
    (set_local $f_args (get_local $res))

    ;; if error, return f/args for release by caller
    (if (get_global $error_type) (return (get_local $f_args)))

    ;; rest
    (set_local $args (call $MEM_VAL0_ptr (get_local $f_args)))
    ;; value
    (set_local $f (call $MEM_VAL1_ptr (get_local $f_args)))

    (set_local $ftype (call $TYPE (get_local $f)))
    (if (i32.eq (get_local $ftype) (get_global $FUNCTION_T))
      (then
        (set_local $res (call_indirect (type $fnT) (get_local $args)
                                       (call $VAL0 (get_local $f)))))
      (else
        (call $THROW_STR_1 (STRING "apply of non-function type: %d\n")
              (get_local $type))
        (set_local $res (i32.const 0))))

    (call $RELEASE (get_local $f_args))

    (get_local $res)
  )

  (func $PRINT (param $ast i32) (result i32)
    (call $pr_str (get_local $ast))
  )

  ;; REPL
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

    ;; release memory from MAL_READ and EVAL
    (call $RELEASE (get_local $mv2))
    (call $RELEASE (get_local $mv1))
    (get_local $ms)
  )

  (func $add (param $args i32) (result i32)
    (call $INTEGER
      (i32.add (call $VAL0 (call $MEM_VAL1_ptr (get_local $args)))
               (call $VAL0 (call $MEM_VAL1_ptr
                                 (call $MEM_VAL0_ptr (get_local $args)))))))
  (func $subtract (param $args i32) (result i32)
    (call $INTEGER
      (i32.sub_s (call $VAL0 (call $MEM_VAL1_ptr (get_local $args)))
                 (call $VAL0 (call $MEM_VAL1_ptr
                                   (call $MEM_VAL0_ptr (get_local $args)))))))
  (func $multiply (param $args i32) (result i32)
    (call $INTEGER
      (i32.mul_s (call $VAL0 (call $MEM_VAL1_ptr (get_local $args)))
                 (call $VAL0 (call $MEM_VAL1_ptr
                                   (call $MEM_VAL0_ptr (get_local $args)))))))
  (func $divide (param $args i32) (result i32)
    (call $INTEGER
      (i32.div_s (call $VAL0 (call $MEM_VAL1_ptr (get_local $args)))
                 (call $VAL0 (call $MEM_VAL1_ptr
                                   (call $MEM_VAL0_ptr (get_local $args)))))))

  (func $main (result i32)
    ;; Constant location/value definitions
    (local $line i32)
    (local $res i32)
    (local $repl_env i32)

    ;; DEBUG
    (call $printf_1 (STRING "memoryBase: %d\n") (get_global $memoryBase))
    (call $printf_1 (STRING "heap_start: %d\n") (get_global $heap_start))
    (call $printf_1 (STRING "heap_end: %d\n") (get_global $heap_end))
    (call $printf_1 (STRING "mem: %d\n") (get_global $mem))
;;    (call $printf_1 (STRING "string_mem: %d\n") (get_global $string_mem))

    (set_local $repl_env (call $HASHMAP))

    (set_local $repl_env (call $ASSOC1_S (get_local $repl_env)
                               (STRING "+") (call $FUNCTION (i32.const 0))))
    (set_local $repl_env (call $ASSOC1_S (get_local $repl_env)
                               (STRING "-") (call $FUNCTION (i32.const 1))))
    (set_local $repl_env (call $ASSOC1_S (get_local $repl_env)
                               (STRING "*") (call $FUNCTION (i32.const 2))))
    (set_local $repl_env (call $ASSOC1_S (get_local $repl_env)
                               (STRING "/") (call $FUNCTION (i32.const 3))))

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
        (set_local $res (call $rep (get_local $line) (get_local $repl_env)))
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

