(module $reader

  ;; TODO: global warning
  (global $token       (mut i32) (i32.const 0))
  (global $read_index  (mut i32) (i32.const 0))

  (func $skip_spaces (param $str i32) (result i32)
    (local $found i32)
    (local $c i32)
    (set_local $found (i32.const 0))
    (set_local $c (i32.load8_u (i32.add (get_local $str)
                                        (get_global $read_index))))
    (block $done
      (loop $loop
        ;;; while (c == ' ' || c == ',' || c == '\n')
        (if (i32.and (i32.and
                       (i32.ne (get_local $c) (CHAR " "))
                       (i32.ne (get_local $c) (CHAR ",")))
                     (i32.ne (get_local $c) (CHAR "\n")))
          (br $done))
        (set_local $found (i32.const 1))
        ;;; c=str[++(*index)]
        (set_global $read_index (i32.add (get_global $read_index)
                                         (i32.const 1)))
        (set_local $c (i32.load8_u (i32.add (get_local $str)
                                            (get_global $read_index))))
        (br $loop)
      )
    )
;;    (call $debug (STRING ">>> skip_spaces:") (get_local $found))
    (get_local $found)
  )

  (func $skip_to_eol (param $str i32) (result i32)
    (local $found i32)
    (local $c i32)
    (set_local $found (i32.const 0))
    (set_local $c (i32.load8_c (i32.add (get_local $str)
                                        (get_global $read_index))))
    (if (i32.eq (get_local $c) (CHAR ";"))
      (then
        (set_local $found (i32.const 1))
        (block $done
          (loop $loop
            ;;; c=str[++(*index)]
            (set_global $read_index (i32.add (get_global $read_index)
                                             (i32.const 1)))
            (set_local $c (i32.load8_u (i32.add (get_local $str)
                                                (get_global $read_index))))
            ;;; while (c != '\0' && c != '\n')
            (if (i32.and (i32.ne (get_local $c) (CHAR "\x00"))
                         (i32.ne (get_local $c) (CHAR "\n")))
              (br $loop))
          )
        )))
;;    (call $debug (STRING ">>> skip_to_eol:") (get_local $found))
    (get_local $found)
  )

  (func $skip_spaces_comments (param $str i32)
    (loop $loop
      ;; skip spaces
      (if (call $skip_spaces (get_local $str)) (br $loop))
      ;; skip comments
      (if (call $skip_to_eol (get_local $str)) (br $loop))
    )
  )

  (func $read_token (param $str i32) (result i32)
    (local $token_index i32)
    (local $instring i32)
    (local $escaped i32)
    (local $c i32)
    (set_local $token_index (i32.const 0))
    (set_local $instring (i32.const 0))
    (set_local $escaped (i32.const 0))

    (call $skip_spaces_comments (get_local $str))

    ;; read first character
    ;;; c=str[++(*index)]
    (set_local $c (i32.load8_u (i32.add (get_local $str)
                                        (get_global $read_index))))
    (set_global $read_index (i32.add (get_global $read_index)
                                     (i32.const 1)))
    ;; read first character
    ;;; token[token_index++] = c
    (i32.store8_u (i32.add (get_global $token) (get_local $token_index))
                  (get_local $c))
    (set_local $token_index (i32.add (get_local $token_index)
                                     (i32.const 1)))
    ;; single/double character token
    (if (i32.or (i32.eq (get_local $c) (CHAR "("))
        (i32.or (i32.eq (get_local $c) (CHAR ")"))
        (i32.or (i32.eq (get_local $c) (CHAR "["))
        (i32.or (i32.eq (get_local $c) (CHAR "]"))
        (i32.or (i32.eq (get_local $c) (CHAR "{"))
        (i32.or (i32.eq (get_local $c) (CHAR "}"))
        (i32.or (i32.eq (get_local $c) (CHAR "'"))
        (i32.or (i32.eq (get_local $c) (CHAR "`"))
        (i32.or (i32.eq (get_local $c) (CHAR "@"))
        (i32.and (i32.eq (get_local $c) (CHAR "~"))
                 (i32.eq (i32.load8_u (i32.add (get_local $str)
                                               (get_global $read_index)))
                         (CHAR "@"))))))))))))

      (then
        ;; continue
        (nop))
      (else
        ;;; if (c == '"') instring = true
        (set_local $instring (i32.eq (get_local $c) (CHAR "\"")))
        (block $done
          (loop $loop
            ;; peek at next character
            ;;; c = str[*index]
            (set_local $c (i32.load8_u (i32.add (get_local $str)
                                                (get_global $read_index))))
            ;;; if (c == '\0') break
            (if (i32.eq (get_local $c) (i32.const 0)) (br $done))
            ;;; if (!instring)
            (if (i32.eqz (get_local $instring))
              (then
                ;; next character is token delimiter
                (if (i32.or (i32.eq (get_local $c) (CHAR "("))
                    (i32.or (i32.eq (get_local $c) (CHAR ")"))
                    (i32.or (i32.eq (get_local $c) (CHAR "["))
                    (i32.or (i32.eq (get_local $c) (CHAR "]"))
                    (i32.or (i32.eq (get_local $c) (CHAR "{"))
                    (i32.or (i32.eq (get_local $c) (CHAR "}"))
                    (i32.or (i32.eq (get_local $c) (CHAR " "))
                    (i32.or (i32.eq (get_local $c) (CHAR ","))
                            (i32.eq (get_local $c) (CHAR "\n"))))))))))
                  (br $done))))
            ;; read next character
            ;;; token[token_index++] = str[(*index)++]
            (i32.store8_u (i32.add (get_global $token)
                                   (get_local $token_index))
                          (i32.load8_u (i32.add (get_local $str)
                                                (get_global $read_index))))
            (set_local $token_index (i32.add (get_local $token_index)
                                             (i32.const 1)))
            (set_global $read_index (i32.add (get_global $read_index)
                                             (i32.const 1)))
            ;;; if (token[0] == '~' && token[1] == '@') break
            (if (i32.and (i32.eq (i32.load8_u (i32.add (get_global $token)
                                                       (i32.const 0)))
                                 (CHAR "~"))
                         (i32.eq (i32.load8_u (i32.add (get_global $token)
                                                       (i32.const 1)))
                                 (i32.const 0x40)))
              (br $done))

            ;;; if ((!instring) || escaped)
            (if (i32.or (i32.eqz (get_local $instring))
                        (get_local $escaped))
              (then
                (set_local $escaped (i32.const 0))
                (br $loop)))
            (if (i32.eq (get_local $c) (CHAR "\\"))
              (set_local $escaped (i32.const 1)))
            (if (i32.eq (get_local $c) (CHAR "\""))
              (br $done))
            (br $loop)
          )
        )))

    ;;; token[token_index] = '\0'
    (i32.store8_u (i32.add (get_global $token) (get_local $token_index))
                  (i32.const 0))
    (get_global $token)
  )

  (func $read_seq (param $str i32) (param $type i32) (param $end i32)
        (result i32)
    (local $res i32)
    (local $val2 i32)
    (local $val3 i32)
    (local $c i32)

    ;; MAP_LOOP stack
    (local $ret i32)
    (local $empty i32)
    (local $current i32)

    ;; MAP_LOOP_START
    (set_local $res (call $MAP_LOOP_START (get_local $type)))
    ;; push MAP_LOOP stack
    ;;; empty = current = ret = res
    (set_local $ret (get_local $res))
    (set_local $current (get_local $res))
    (set_local $empty (get_local $res))

    ;; READ_SEQ_LOOP
    (block $done
      (loop $loop
        (call $skip_spaces_comments (get_local $str))

        ;; peek at next character
        ;;; c = str[*index]
        (set_local $c (i32.load8_u (i32.add (get_local $str)
                                            (get_global $read_index))))
        (if (i32.eq (get_local $c) (CHAR "\x00"))
          (then
            (call $THROW_STR_0 (STRING "unexpected EOF"))
            (br $done)))
        (if (i32.eq (get_local $c) (get_local $end))
          (then
            ;; read next character
            ;;; c = str[(*index)++]
            (set_local $c (i32.load8_u (i32.add (get_local $str)
                                                (get_global $read_index))))
            (set_global $read_index (i32.add (get_global $read_index)
                                             (i32.const 1)))
            (br $done)))

        ;; value (or key for hash-maps)
        (set_local $val2 (call $read_form (get_local $str)))

        ;; if error, release the unattached element
        (if (get_global $error_type)
          (then
            (call $RELEASE (get_local $val2))
            (br $done)))

        ;; if this is a hash-map, READ_FORM again
        (if (i32.eq (get_local $type) (get_global $HASHMAP_T))
          (set_local $val3 (call $read_form (get_local $str))))

        ;; update the return sequence structure
        ;; MAP_LOOP_UPDATE
        (set_local $res (call $MAP_LOOP_UPDATE (get_local $type)
                              (get_local $empty) (get_local $current)
                              (get_local $val2) (get_local $val3)))
        (if (i32.le_u (get_local $current) (get_global $EMPTY_HASHMAP))
          ;; if first element, set return to new element
          (set_local $ret (get_local $res)))
        ;; update current to point to new element
        (set_local $current (get_local $res))

        (br $loop)
      )
    )

    ;; MAP_LOOP_DONE
    (get_local $ret)
  )

  (func $read_form (param $str i32) (result i32)
    ;;(call $STRING (get_global $STRING_T) (get_local $str))
    (local $tok i32)
    (local $c0 i32)
    (local $c1 i32)
    (local $res i32)

    (if (get_global $error_type) (return (i32.const 0)))

    (set_local $tok (call $read_token (get_local $str)))
;;    (call $debug (STRING ">>> read_form 1:") (get_local $tok))
    ;;; c0 = token[0]
    (set_local $c0 (i32.load8_u (get_local $tok)))
    (set_local $c1 (i32.load8_u (i32.add (get_local $tok) (i32.const 1))))

    (if (i32.eq (get_local $c0) (i32.const 0))
      (then
        (return (call $INC_REF (get_global $NIL))))
    (else (if (i32.or
                (i32.and
                  (i32.ge_u (get_local $c0) (CHAR "0"))
                  (i32.le_u (get_local $c0) (CHAR "9")))
                (i32.and
                  (i32.eq (get_local $c0) (CHAR "-"))
                  (i32.and (i32.ge_u (get_local $c1) (CHAR "0"))
                           (i32.le_u (get_local $c1) (CHAR "9")))))
      (then
        (return (call $INTEGER (call $ATOI (get_local $tok)))))
    (else (if (i32.eq (get_local $c0) (CHAR ":"))
      (then
        (i32.store8_u (get_local $tok) (CHAR "\x7f"))
        (return (call $STRING (get_global $STRING_T) (get_local $tok))))
    (else (if (i32.eq (get_local $c0) (CHAR "\""))
      (then
        ;; TODO: unescape
        (i32.store8_u (i32.sub_u
                        (i32.add (get_local $tok)
                                 (call $STRING_LEN (get_local $tok)))
                        (i32.const 1))
                      (CHAR "\x00"))
        (return (call $STRING (get_global $STRING_T) (i32.add (get_local $tok)
                                                              (i32.const 1)))))
    (else (if (i32.eq (get_local $c0) (CHAR "("))
      (then
        (return (call $read_seq (get_local $str)
                      (get_global $LIST_T) (CHAR ")"))))
    (else (if (i32.eq (get_local $c0) (CHAR "["))
      (then
        (return (call $read_seq (get_local $str)
                      (get_global $VECTOR_T) (CHAR "]"))))
    (else (if (i32.eq (get_local $c0) (CHAR "{"))
      (then
        (return (call $read_seq (get_local $str)
                      (get_global $HASHMAP_T) (CHAR "}"))))
    (else (if (i32.or (i32.eq (get_local $c0) (CHAR ")"))
                      (i32.or (i32.eq (get_local $c0) (CHAR "]"))
                              (i32.eq (get_local $c0) (CHAR "}"))))
      (then
        (call $THROW_STR_1 (STRING "unexpected '%c'") (get_local $c0))
        (return (i32.const 0)))
    (else
      (return (call $STRING (get_global $SYMBOL_T)
                    (get_local $tok)))))))))))))))))))
  )

  (func $read_str (param $str i32) (result i32)
    (set_global $read_index (i32.const 0))
    (call $read_form (get_local $str))
  )

  (export "read_str" (func $read_str))

)
