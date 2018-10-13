(module $util
  (import "env" "malloc" (func $malloc (param i32) (result i32)))
  (import "env" "free" (func $free (param i32)))
  (import "env" "exit" (func $exit (param i32)))

  (import "env" "stdout" (global $stdout i32))
  (import "env" "putchar" (func $putchar (param i32) (result i32)))
  (import "env" "fputs" (func $fputs (param i32 i32) (result i32)))
  ;;(import "env" "readline" (func $readline (param i32) (result i32)))
  (import "libedit.so" "readline" (func $readline (param i32) (result i32)))
  ;;(import "libreadline.so" "readline" (func $readline (param i32) (result i32)))

  (global $sprintf_buf (mut i32) (i32.const 0))

  (func $init_sprintf_mem
    ;; 256 character sprintf static buffer
    (set_global $sprintf_buf (STRING "                                                                                                                                                                                                                                                                "))
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Copy len chatacters from src to dst
  ;; Returns len
  (func $MEM_COPY (param $dst i32) (param $src i32) (param $len i32)
    (local $idx i32)
    (set_local $idx (i32.const 0))
    (loop $copy
          (i32.store8_u (i32.add (get_local $idx) (get_local $dst))
                        (i32.load8_u (i32.add (get_local $idx)
                                              (get_local $src))))
      (set_local $idx (i32.add (i32.const 1) (get_local $idx)))
      (br_if $copy (i32.lt_u (get_local $idx) (get_local $len)))
    )
  )

  (func $STRING_LEN (param $str i32) (result i32)
    (local $cur i32)
    (set_local $cur (get_local $str))
    (loop $count
      (if (i32.ne (i32.const 0) (i32.load8_u (get_local $cur)))
        (then
          (set_local $cur (i32.add (get_local $cur) (i32.const 1)))
          (br $count)))
    )
    (i32.sub_u (get_local $cur) (get_local $str))
  )

  (func $ATOI (param $str i32) (result i32)
    (local $acc i32)
    (local $i i32)
    (local $neg i32)
    (local $ch i32)
    (set_local $acc (i32.const 0))
    (set_local $i (i32.const 0))
    (set_local $neg (i32.const 0))
    (block $done
      (loop $loop
        (set_local $ch (i32.load8_u (i32.add (get_local $str)
                                             (get_local $i))))
        (if (i32.and (i32.ne (get_local $ch) (CHAR '-'))
                     (i32.or (i32.lt_u (get_local $ch) (CHAR '0'))
                             (i32.gt_u (get_local $ch) (CHAR '9'))))
          (br $done))
        (set_local $i (i32.add (get_local $i) (i32.const 1)))
        (if (i32.eq (get_local $ch) (CHAR '-'))
          (then
            (set_local $neg (i32.const 1)))
          (else
            (set_local $acc (i32.add (i32.mul_u (get_local $acc) (i32.const 10))
                                     (i32.sub_u (get_local $ch) (CHAR '0'))))))
        (br $loop)
      )
    )
    (if i32 (get_local $neg)
      (then (i32.sub_s (i32.const 0) (get_local $acc)))
      (else (get_local $acc)))
  )

  (func $strcmp (param $s1 i32) (param $s2 i32) (result i32)
    (block $done
      (loop $loop
        (if (i32.or (i32.eqz (i32.load8_u (get_local $s1)))
                    (i32.eqz (i32.load8_u (get_local $s2))))
          (br $done))
        (if (i32.ne (i32.load8_u (get_local $s1))
                    (i32.load8_u (get_local $s2)))
          (br $done))
        (set_local $s1 (i32.add (get_local $s1) (i32.const 1)))
        (set_local $s2 (i32.add (get_local $s2) (i32.const 1)))
        (br $loop)
      )
    )
    (if i32 (i32.eq (i32.load8_u (get_local $s1))
                    (i32.load8_u (get_local $s2)))
      (then (i32.const 0))
      (else
        (if i32 (i32.lt_u (i32.load8_u (get_local $s1))
                          (i32.load8_u (get_local $s2)))
          (then (i32.const -1))
          (else (i32.const 1)))))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $print (param $addr i32)
    (drop (call $fputs (get_local $addr) (get_global $stdout))))

  (func $printf_1 (param $fmt i32)
        (param $v0 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (i32.const 0) (i32.const 0)
                (i32.const 0) (i32.const 0) (i32.const 0)))
    (call $print (get_global $sprintf_buf))
  )

  (func $printf_2 (param $fmt i32)
        (param $v0 i32) (param $v1 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (get_local $v1) (i32.const 0)
                (i32.const 0) (i32.const 0) (i32.const 0)))
    (call $print (get_global $sprintf_buf))
  )

  (func $printf_3 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (get_local $v1) (get_local $v2)
                (i32.const 0) (i32.const 0) (i32.const 0)))
    (call $print (get_global $sprintf_buf))
  )

  (func $printf_4 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (get_local $v1) (get_local $v2)
                (get_local $v3) (i32.const 0) (i32.const 0)))
    (call $print (get_global $sprintf_buf))
  )

  (func $printf_5 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (get_local $v1) (get_local $v2)
                (get_local $v3) (get_local $v4) (i32.const 0)))
    (call $print (get_global $sprintf_buf))
  )

  (func $printf_6 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32) (param $v5 i32)
    (drop (call $sprintf_6 (get_global $sprintf_buf) (get_local $fmt)
                (get_local $v0) (get_local $v1) (get_local $v2)
                (get_local $v3) (get_local $v4) (get_local $v5)))
    (call $print (get_global $sprintf_buf))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $_sprintdigit (param $str i32) (param $num i32) (param $base i32)
    (local $n i32)
    (local $ch i32)
    (set_local $n (i32.rem_u (get_local $num) (get_local $base)))
    (set_local $ch (if (result i32) (i32.lt_u (get_local $n) (i32.const 10))
                     (i32.const 48)
                     (i32.const 55)))
    (i32.store8_u (get_local $str) (i32.add (get_local $n) (get_local $ch)))
  )

  ;; TODO: switch to snprint* (add buffer len)
  (func $_sprintnum (param $str i32) (param $num i32) (param $base i32)
                  (result i32)
    (if (i32.and (i32.eq (get_local $base) (i32.const 10))
                 (i32.lt_s (get_local $num) (i32.const 0)))
      (then
        ;; Print '-' if negative
        (i32.store8_u (get_local $str) (CHAR '-'))
        (set_local $str (i32.add (get_local $str) (i32.const 1)))
        ;; Reverse the sign
        (set_local $num (i32.sub_s (i32.const 0) (get_local $num)))))
    (if (i32.gt_u (i32.div_u (get_local $num) (get_local $base))
                  (i32.const 0))
      (set_local
        $str
        (call $_sprintnum (get_local $str)
              (i32.div_u (get_local $num) (get_local $base))
              (get_local $base))))
    (call $_sprintdigit (get_local $str) (get_local $num) (get_local $base))
    (i32.add (i32.const 1) (get_local $str))
  )

  ;; TODO: switch to snprint* (add buffer len)
  (func $sprintf_1 (param $str i32) (param $fmt i32)
        (param $v0 i32) (result i32)
    (call $sprintf_6 (get_local $str) (get_local $fmt)
          (get_local $v0) (i32.const 0) (i32.const 0)
          (i32.const 0) (i32.const 0) (i32.const 0))
  )

  (func $sprintf_6 (param $str i32) (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32) (param $v5 i32)
        (result i32)
    (local $ch i32)
    (local $pstr i32)
    (local $v i32)
    (local $vidx i32)
    (local $len i32)
    (set_local $pstr (get_local $str))
    (set_local $vidx (i32.const 0))

    (block $done
      (loop $loop
        (block $after_v
          (block (block (block (block (block (block
          (br_table 0 1 2 3 4 5 0 (get_local $vidx)))
          (; 0 ;) (set_local $v (get_local $v0)) (br $after_v))
          (; 1 ;) (set_local $v (get_local $v1)) (br $after_v))
          (; 2 ;) (set_local $v (get_local $v2)) (br $after_v))
          (; 3 ;) (set_local $v (get_local $v3)) (br $after_v))
          (; 4 ;) (set_local $v (get_local $v4)) (br $after_v))
          (; 5 ;) (set_local $v (get_local $v5)) (br $after_v)
        )

        ;;; while ((ch=*(fmt++)))
        (set_local $ch (i32.load8_u (get_local $fmt)))
        (set_local $fmt (i32.add (i32.const 1) (get_local $fmt)))
        (if (i32.eqz (get_local $ch)) (br $done))
        ;; TODO: check buffer length

        (if (i32.ne (get_local $ch) (CHAR '%'))
          (then
            ;; TODO: check buffer length
            (i32.store8_u (get_local $pstr) (get_local $ch))
            (set_local $pstr (i32.add (i32.const 1) (get_local $pstr)))
            (br $loop)))

        ;;; ch=*(fmt++)
        (set_local $ch (i32.load8_u (get_local $fmt)))
        (set_local $fmt (i32.add (i32.const 1) (get_local $fmt)))

        (if (i32.eq (CHAR 'd') (get_local $ch))
          (then
            (set_local $pstr (call $_sprintnum (get_local $pstr)
                                   (get_local $v) (i32.const 10))))
        (else (if (i32.eq (CHAR 'x') (get_local $ch))
          (then
            (set_local $pstr (call $_sprintnum (get_local $pstr)
                                   (get_local $v) (i32.const 16))))
        (else (if (i32.eq (CHAR 's') (get_local $ch))
          (then
            (set_local $len (call $STRING_LEN (get_local $v)))
            (call $MEM_COPY (get_local $pstr) (get_local $v) (get_local $len))
            (set_local $pstr (i32.add (get_local $pstr) (get_local $len))))
        (else (if (i32.eq (CHAR 'c') (get_local $ch))
          (then
            (i32.store8_u (get_local $pstr) (get_local $v))
            (set_local $pstr (i32.add (get_local $pstr) (i32.const 1))))
        (else
          (call $print (STRING "Illegal format character: "))
          (drop (call $putchar (get_local $ch)))
          (drop (call $putchar (CHAR '\n')))
          (call $exit (i32.const 3))))))))))

        (set_local $vidx (i32.add (i32.const 1) (get_local $vidx)))
        (br $loop)
      )
    )

    (i32.store8_u (get_local $pstr) (CHAR '\x00'))
    (get_local $pstr)
  )

)

