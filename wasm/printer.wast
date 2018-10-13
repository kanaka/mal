(module $printer

  (func $pr_str_val (param $res i32) (param $mv i32) (result i32)
    (local $type i32)
    (local $val0 i32)
    (local $sval i32)
    (set_local $type (call $TYPE (get_local $mv)))
    (set_local $val0 (call $MalVal_val (call $MalVal_index (get_local $mv))
                           (i32.const 0)))

    ;;; switch(type)
    (block $done
      (block $default
      (block (block (block (block (block (block (block (block
      (block (block (block (block (block (block (block (block
      (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 (get_local $type)))
      ;; 0: nil
      (call $MEM_COPY (get_local $res) (STRING "nil") (i32.const 4))
      (set_local $res (i32.add (i32.const 3) (get_local $res)))
      (br $done))
      ;; 1: boolean
      (if (i32.eq (get_local $val0) (i32.const 0))
        (then
          ;; false
          (call $MEM_COPY (get_local $res) (STRING "false") (i32.const 5))
          (set_local $res (i32.add (i32.const 5) (get_local $res))))
        (else
          ;; true
          (call $MEM_COPY (get_local $res) (STRING "true") (i32.const 4))
          (set_local $res (i32.add (i32.const 4) (get_local $res)))))
      (br $done))
      ;; 2: integer
      (set_local $res (call $sprintf_1 (get_local $res) (STRING "%d")
                            (get_local $val0)))
      (br $done))
      ;; 3: float/ERROR
      (set_local $res (call $sprintf_1 (get_local $res) (STRING "%d")
                            (STRING " *** GOT FLOAT *** ")))
      (br $done))
      ;; 4: string/kw
      (set_local $sval (call $to_String (get_local $mv)))
      (if (i32.eq (i32.load8_u (get_local $sval)) (CHAR "\x7f"))
        (then
          (set_local $res (call $sprintf_1 (get_local $res) (STRING ":%s")
                                (i32.add (get_local $sval) (i32.const 1)))))
        (else
          (set_local $res (call $sprintf_1 (get_local $res) (STRING "\"%s\"")
                                (call $to_String (get_local $mv))))))
      (br $done))
      ;; 5: symbol
      (set_local $res (call $sprintf_1 (get_local $res) (STRING "%s")
                            (call $to_String (get_local $mv))))
      (br $done))
      ;; 6: list, fallthrouogh
      )
      ;; 7: vector, fallthrough
      )
      ;; 8: hashmap
      (set_local
        $res (call $sprintf_1 (get_local $res) (STRING "%c")
                   (if i32 (i32.eq (get_local $type) (get_global $LIST_T))
                     (CHAR "(")
                     (else (if i32 (i32.eq (get_local $type) (get_global $VECTOR_T))
                       (CHAR "[")
                       (else (CHAR "{")))))))
      ;; PR_SEQ_LOOP
      ;;; while (VAL0(mv) != 0)
      (block $done_seq
        (loop $seq_loop
          (if (i32.eq (call $VAL0 (get_local $mv)) (i32.const 0))
            (br $done_seq))
          ;;; res = pr_str_val(res, MEM_VAL1(mv), print_readably)
          (set_local $res (call $pr_str_val (get_local $res)
                                (call $MEM_VAL1_ptr (get_local $mv))))

          ;; if this is a hash-map, print the next element
          (if (i32.eq (get_local $type) (get_global $HASHMAP_T))
            (then
              ;;; res += snprintf(res, 2, " ")
              (set_local $res (call $sprintf_1 (get_local $res) (STRING " ")
                                    (i32.const 0)))
              (set_local $res (call $pr_str_val (get_local $res)
                                    (call $MEM_VAL2_ptr (get_local $mv))))))
          ;;; mv = MEM_VAL0(mv)
          (set_local $mv (call $MEM_VAL0_ptr (get_local $mv)))
          ;;; if (VAL0(mv) != 0)
          (if (i32.ne (call $VAL0 (get_local $mv)) (i32.const 0))
            ;;; res += snprintf(res, 2, " ")
            (set_local $res (call $sprintf_1 (get_local $res) (STRING " ")
                                  (i32.const 0))))
          ;;(call $print (STRING "here4\n"))
          (br $seq_loop)
        )
      )

      (set_local
        $res (call $sprintf_1 (get_local $res) (STRING "%c")
                   (if i32 (i32.eq (get_local $type) (get_global $LIST_T))
                     (CHAR ")")
                     (else (if i32 (i32.eq (get_local $type) (get_global $VECTOR_T))
                       (CHAR "]")
                       (else (CHAR "}")))))))
      (br $done))
      ;; 9: function
      (call $MEM_COPY (get_local $res) (STRING "#<fn ...>") (i32.const 10))
      (set_local $res (i32.add (i32.const 9) (get_local $res)))
      (br $done))
      ;; 10: mal function
      (call $MEM_COPY (get_local $res) (STRING "(fn* ...)") (i32.const 10))
      (set_local $res (i32.add (i32.const 9) (get_local $res)))
      (br $done))
      ;; 11: macro fn
      (call $print (STRING "macro fn"))
      (call $MEM_COPY (get_local $res) (STRING "#<macro ...>") (i32.const 13))
      (set_local $res (i32.add (i32.const 12) (get_local $res)))
      (br $done))
      ;; 12: atom
      (call $MEM_COPY (get_local $res) (STRING "(atom ...)") (i32.const 11))
      (set_local $res (i32.add (i32.const 10) (get_local $res)))
      (br $done))
      ;; 13: environment
      (call $MEM_COPY (get_local $res) (STRING "#<mem ...>") (i32.const 11))
      (set_local $res (i32.add (i32.const 10) (get_local $res)))
      (br $done))
      ;; 14: metadata
      (call $MEM_COPY (get_local $res) (STRING "#<meta ...>") (i32.const 12))
      (set_local $res (i32.add (i32.const 11) (get_local $res)))
      (br $done))
      ;; 15: FREE
      (call $MEM_COPY (get_local $res) (STRING "#<free ...>") (i32.const 12))
      (set_local $res (i32.add (i32.const 11) (get_local $res)))
      (br $done))
      ;; 16: default
      (call $MEM_COPY (get_local $res) (STRING "#<unknown>") (i32.const 11))
      (set_local $res (i32.add (i32.const 10) (get_local $res)))
    )

    (get_local $res)
  )

  (func $pr_str (param $mv i32) (result i32)
    (drop (call $pr_str_val (get_global $sprintf_buf) (get_local $mv)))
    (call $STRING (get_global $STRING_T) (get_global $sprintf_buf))
  )

  (export "pr_str" (func $pr_str))

)
