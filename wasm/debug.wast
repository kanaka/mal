(module $debug

  (func $PR_VALUE (param $fmt i32) (param $mv i32)
    (local $temp i32)
    (set_local $temp (call $pr_str (get_local $mv)))
    (call $printf_1 (get_local $fmt) (call $to_String (get_local $temp)))
    (call $RELEASE (get_local $temp))
  )

  (func $PR_MEMORY_VALUE (param $idx i32) (result i32)
    (local $mv i32)
    (local $type i32)
    (local $size i32)
    (local $val0 i32)
    ;;; mv = mem + idx
    (set_local $mv (call $MalVal_ptr (get_local $idx)))
    (set_local $type (call $TYPE (get_local $mv)))
    (set_local $size (call $MalVal_size (get_local $mv)))
    (set_local $val0 (call $MalVal_val (get_local $idx) (i32.const 0)))

    ;;; printf(" %3d: type: %2d", idx, type)
    (call $printf_2 (STRING " 0x%x: type: %d")
          (get_local $idx) (get_local $type))

    (if (i32.eq (get_local $type) (i32.const 15))
      (then
        ;;; printf(", size: %2d", size)
        (call $printf_1 (STRING ", size: %d") (get_local $size)))
      (else
        ;;;  printf(", refs: %2d", (mv->refcnt_type - type)>>5)
        (call $printf_1 (STRING ", refs: %d") (call $REFS (get_local $mv)))))

    ;;; printf(", [ %3d | %3d", mv->refcnt_type, val0)
    (call $printf_2 (STRING ", [ 0x%x | 0x%x")
          (call $MalVal_refcnt_type (get_local $idx))
          (get_local $val0))

    (if (i32.eq (get_local $size) (i32.const 2))
      (then
        (call $print (STRING " | --- | --- ]")))
      (else
        ;;; printf(" | %3d", mv->val[1])
        (call $printf_1 (STRING " | 0x%x")
              (call $MalVal_val (get_local $idx) (i32.const 1)))
        (if (i32.eq (get_local $size) (i32.const 3))
          (then
            (call $print (STRING " | --- ]")))
          (else
            ;;; printf(" | %3d ]", mv->val[2])
            (call $printf_1 (STRING " | 0x%x ]")
                  (call $MalVal_val (get_local $idx) (i32.const 2)))))))

    ;;; printf(" >> ")
    (call $print (STRING " >> "))

    (block $done (block $unknown
    (block (block (block (block (block (block (block (block
    (block (block (block (block (block (block (block (block
      (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 $unknown (get_local $type)))
    ;; 0: nil
    (call $print (STRING "nil"))
    (br $done))
    ;; 1: boolean
    (if (i32.eq (get_local $val0) (i32.const 0))
      ;; true
      (call $print (STRING "false"))
      ;; false
      (call $print (STRING "true")))
    (br $done))
    ;; 2: integer
    (call $printf_1 (STRING "%d") (get_local $val0))
    (br $done))
    ;; 3: float/ERROR
    (call $print (STRING " *** GOT FLOAT *** "))
    (br $done))
    ;; 4: string/kw
    (call $printf_1 (STRING "'%s'") (call $to_String (get_local $mv)))
    (br $done))
    ;; 5: symbol
    (call $print (call $to_String (get_local $mv)))
    (br $done))
    ;; 6: list
    (if (i32.le_u (get_local $mv) (get_global $EMPTY_HASHMAP))
      (then
        (call $print (STRING "()")))
      (else
        ;;; printf("(... %d ...), next: %d\n", mv->val[1], mv->val[0])
        (call $printf_2 (STRING "(... 0x%x ...), next: 0x%x")
              (call $MalVal_val (get_local $idx) (i32.const 1))
              (call $MalVal_val (get_local $idx) (i32.const 0)))))
    (br $done))
    ;; 7: vector
    (if (i32.le_u (get_local $mv) (get_global $EMPTY_HASHMAP))
      (then
        (call $print (STRING "[]")))
      (else
        ;;; printf("[... %d ...], next: %d\n", mv->val[1], mv->val[0])val
        (call $printf_2 (STRING "[... %d ...], next: %d")
              (call $MalVal_val (get_local $idx) (i32.const 1))
              (call $MalVal_val (get_local $idx) (i32.const 0)))))
    (br $done))
    ;; 8: hashmap
    (if (i32.le_u (get_local $mv) (get_global $EMPTY_HASHMAP))
      (then
        (call $print (STRING "{}")))
      (else
        ;;; printf("{... '%s'(%d) : %d ...}\n",
        ;;         to_String(mem + mv->val[1]), mv->val[1], mv->val[2])
        (call $printf_3 (STRING "{... '%s'(%d) : %d ...}")
              (call $to_String
                    (call $MalVal_ptr
                          (call $MalVal_val (get_local $idx) (i32.const 1))))
              (call $MalVal_val (get_local $idx) (i32.const 1))
              (call $MalVal_val (get_local $idx) (i32.const 2)))))
    (br $done))
    ;; 9: function
    (call $print (STRING "function"))
    (br $done))
    ;; 10: mal function
    (call $print (STRING "mal function"))
    (br $done))
    ;; 11: macro fn
    (call $print (STRING "macro fn"))
    (br $done))
    ;; 12: atom
    (call $print (STRING "atom"))
    (br $done))
    ;; 13: environment
    (call $print (STRING "environment"))
    (br $done))
    ;; 14: metadata
    (call $print (STRING "metadata"))
    (br $done))
    ;; 15: FREE
    (call $printf_1 (STRING "FREE next: 0x%x") (get_local $val0))
    (if (i32.eq (get_local $idx) (get_global $mem_free_list))
      (call $print (STRING " (free start)")))
    (if (i32.eq (get_local $val0) (get_global $mem_unused_start))
      (call $print (STRING " (free end)")))
    (br $done))
    ;; 16: unknown
    (call $print (STRING "unknown"))
    )

    (drop (call $putchar (i32.const 0xA)))

    (i32.add (get_local $size) (get_local $idx))
  )

  (func $PR_MEMORY (param $start i32) (param $end i32)
    (local $idx i32)
    (if (i32.lt_s (get_local $start) (i32.const 0))
      (set_local $start (get_global $mem_user_start)))
    (if (i32.lt_s (get_local $end) (i32.const 0))
      (set_local $end (get_global $mem_unused_start)))
    ;;; printf("Values - (mem) showing %d -> %d", start, end)
    ;;; printf(" (unused start: %d, free list: %d):\n",
    ;;;        mem_unused_start, mem_free_list)
    (call $printf_4 (STRING "Values - (mem) showing 0x%x -> 0x%x (unused start: 0x%x, free list: 0x%x):\n")
          (get_local $start)
          (get_local $end)
          (get_global $mem_unused_start)
          (get_global $mem_free_list))

    (if (i32.le_s (get_local $end) (get_local $start))
      (then
        (call $print (STRING "  ---\n"))
        (set_local $end (get_global $mem_unused_start)))
      (else
        (set_local $idx (get_local $start))
        ;;; while (idx < end)
        (block $loopvals_exit
          (loop $loopvals
            (if (i32.ge_s (get_local $idx) (get_local $end))
              (br $loopvals_exit))
            (set_local $idx (call $PR_MEMORY_VALUE (get_local $idx)))
            (br $loopvals)
          )
        )))
  )

  (func $PR_MEMORY_RAW (param $start i32) (param $end i32)
    (block $loop_exit
      (loop $loop
        (if (i32.ge_u (get_local $start) (get_local $end))
          (br $loop_exit))
        (call $printf_2 (STRING "0x%x 0x%x\n")
              (get_local $start) (i32.load (get_local $start)))
        (set_local $start (i32.add (i32.const 4) (get_local $start)))
        (br $loop)
      )
    )
  )
)
