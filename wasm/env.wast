(module $env

  (func $ENV_NEW (param $outer i32) (result i32)
    (local $data i32)
    (local $env i32)

    ;; allocate the data hashmap
    (set_local $data (call $HASHMAP))

    (set_local $env (call $ALLOC (get_global $ENVIRONMENT_T)
                          (get_local $data) (get_local $outer) (i32.const 0)))
    ;; environment takes ownership
    (call $RELEASE (get_local $data))
    (get_local $env)
  )

  (func $ENV_SET (param $env i32) (param $key i32) (param $value i32)
        (result i32)
    (local $data i32)
    (set_local $data (call $MEM_VAL0_ptr (get_local $env)))
    (i32.store (call $VAL0_ptr (get_local $env))
               (call $MalVal_index
                     (call $ASSOC1 (get_local $data)
                           (get_local $key) (get_local $value))))
    (get_local $value)
  )

  (func $ENV_SET_S (param $env i32) (param $key i32) (param $value i32)
        (result i32)
    (local $data i32)
    (set_local $data (call $MEM_VAL0_ptr (get_local $env)))
    (i32.store (call $VAL0_ptr (get_local $env))
               (call $MalVal_index
                     (call $ASSOC1_S (get_local $data)
                           (get_local $key) (get_local $value))))
    (get_local $value)
  )

  (func $ENV_FIND (param $env i32) (param $key i32) (result i64)
    (local $res i32)
    (local $data i32)
    (local $found_res i64)

    (set_local $res (i32.const 0))

    (block $done
      (loop $loop
        (set_local $data (call $MEM_VAL0_ptr (get_local $env)))
        (set_local $found_res (call $HASHMAP_GET (get_local $data)
                                    (get_local $key)))
        ;;; if (found)
        (if (i32.wrap/i64 (i64.shr_u (get_local $found_res)
                                     (i64.const 32)))
          (then
            (set_local $res (i32.wrap/i64 (get_local $found_res)))
            (br $done)))
        (set_local $env (call $MEM_VAL1_ptr (get_local $env)))
        (if (i32.eq (get_local $env) (get_global $NIL))
          (then
            (set_local $env (i32.const 0))
            (br $done)))
        (br $loop)
      )
    )

    ;; combine res/env as hi 32/low 32 of i64
    (i64.or
      (i64.shl_u (i64.extend_u/i32 (get_local $res))
                 (i64.const 32))
      (i64.extend_u/i32 (get_local $env)))
  )

  (func $ENV_GET (param $env i32) (param $key i32) (result i32)
    (local $res i32)
    (local $res_env i64)
    (set_local $res (i32.const 0))

    (set_local $res_env (call $ENV_FIND (get_local $env) (get_local $key)))
    (set_local $env (i32.wrap/i64 (get_local $res_env)))
    (set_local $res (i32.wrap/i64 (i64.shr_u (get_local $res_env)
					       (i64.const 32))))

    (if (i32.eqz (get_local $env))
      (then
        (call $THROW_STR_1 (STRING "'%s' not found")
              (call $to_String (get_local $key)))
        (return (get_local $res))))
    (return (call $INC_REF (get_local $res)))
  )
)
