;; Mal value memory layout
;;   type           words
;;   ----------     ----------
;;   nil            ref/ 0 |  0           |               |
;;   false          ref/ 1 |  0           |               |
;;   true           ref/ 1 |  1           |               |
;;   integer        ref/ 2 | int          |               |
;;   float          ref/ 3 | ???          |               |
;;   string/kw      ref/ 4 | string ptr   |               |
;;   symbol         ref/ 5 | string ptr   |               |
;;   list           ref/ 6 | next mem idx | val mem idx   |
;;   vector         ref/ 7 | next mem idx | val mem idx   |
;;   hashmap        ref/ 8 | next mem idx | key mem idx   | val mem idx
;;   function       ref/ 9 | fn idx       |               |
;;   mal function   ref/10 | body mem idx | param mem idx | env mem idx
;;   macro fn       ref/11 | body mem idx | param mem idx | env mem idx
;;   atom           ref/12 | val mem idx  |               |
;;   environment    ref/13 | hmap mem idx | outer mem idx |
;;   metadata       ref/14 | obj mem idx  | meta mem idx  |
;;   FREE            sz/15 | next mem idx |               |

(module $types

  (global $NIL_T                  i32 (i32.const 0))
  (global $BOOLEAN_T              i32 (i32.const 1))
  (global $INTEGER_T              i32 (i32.const 2))
  (global $FLOAT_T                i32 (i32.const 3))
  (global $STRING_T               i32 (i32.const 4))
  (global $SYMBOL_T               i32 (i32.const 5))
  (global $LIST_T                 i32 (i32.const 6))
  (global $VECTOR_T               i32 (i32.const 7))
  (global $HASHMAP_T              i32 (i32.const 8))
  (global $FUNCTION_T             i32 (i32.const 9))
  (global $MALFUNC_T              i32 (i32.const 10))
  (global $MACRO_T                i32 (i32.const 11))
  (global $ATOM_T                 i32 (i32.const 12))
  (global $ENVIRONMENT_T          i32 (i32.const 13))
  (global $METADATA_T             i32 (i32.const 14))
  (global $FREE_T                 i32 (i32.const 15))

  (global $error_type             (mut i32) (i32.const 0))
  (global $error_val              (mut i32) (i32.const 0))
  ;; Index into static string memory (static.wast)
  (global $error_str              (mut i32) (i32.const 0))

  (global $NIL                    (mut i32) (i32.const 0))
  (global $FALSE                  (mut i32) (i32.const 0))
  (global $TRUE                   (mut i32) (i32.const 0))
  (global $EMPTY_LIST             (mut i32) (i32.const 0))
  (global $EMPTY_VECTOR           (mut i32) (i32.const 0))
  (global $EMPTY_HASHMAP          (mut i32) (i32.const 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General functions

  (func $INC_REF (param $mv i32) (result i32)
    (i32.store (get_local $mv)
               (i32.add (i32.load (get_local $mv))
                        (i32.const 32)))
    (get_local $mv))

  (func $THROW_STR_0 (param $fmt i32)
    (drop (call $sprintf_1 (get_global $error_str) (get_local $fmt) (STRING "")))
    (set_global $error_type (i32.const 1)))

  (func $THROW_STR_1 (param $fmt i32) (param $v0 i32)
    (drop (call $sprintf_1 (get_global $error_str) (get_local $fmt) (get_local $v0)))
    (set_global $error_type (i32.const 1)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; numeric functions

  (func $INTEGER (param $val i32) (result i32)
    (call $ALLOC_SCALAR (get_global $INTEGER_T) (get_local $val)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sequence functions

  (func $MAP_LOOP_START (param $type i32) (result i32)
    (local $res i32)
    (set_local $res (if i32 (i32.eq (get_local $type)
                                    (get_global $LIST_T))
                      (get_global $EMPTY_LIST)
                    (else (if i32 (i32.eq (get_local $type)
                                            (get_global $VECTOR_T))
                      (get_global $EMPTY_VECTOR)
                    (else (if i32 (i32.eq (get_local $type)
                                          (get_global $HASHMAP_T))
                      (get_global $EMPTY_HASHMAP)
                    (else
                      (call $THROW_STR_1 (STRING "read_seq invalid type %d")
                            (get_local $type))
                      (i32.const 0))))))))

    (call $INC_REF (get_local $res))
  )

  (func $MAP_LOOP_UPDATE (param $type i32) (param $empty i32)
        (param $current i32) (param $val2 i32) (param $val3 i32)
        (result i32)
    (local $res i32)

    (set_local $res (call $ALLOC (get_local $type) (get_local $empty)
                          (get_local $val2) (get_local $val3)))
    ;; sequence took ownership
    (call $RELEASE (get_local $empty))
    (call $RELEASE (get_local $val2))
    (if (i32.eq (get_local $type) (get_global $HASHMAP_T))
      (call $RELEASE (get_local $val3)))
    (if (i32.gt_u (get_local $current) (get_global $EMPTY_HASHMAP))
      ;; if not first element, set current next to point to new element
      (i32.store (call $VAL0_ptr (get_local $current))
                 (call $MalVal_index (get_local $res))))

    (get_local $res)
  )

  (func $EMPTY_Q (param $mv i32) (result i32)
    (i32.eq (call $VAL0 (get_local $mv)) (i32.const 0))
  )

  (func $HASHMAP (result i32)
    ;; just point to static empty hash-map
    (call $INC_REF (get_global $EMPTY_HASHMAP))
  )

  (func $ASSOC1 (param $hm i32) (param $k i32) (param $v i32) (result i32)
    (local $res i32)
    (set_local $res (call $ALLOC (get_global $HASHMAP_T) (get_local $hm)
                          (get_local $k) (get_local $v)))
    ;; we took ownership of previous release
    (call $RELEASE (get_local $hm))
    (get_local $res)
  )

  (func $ASSOC1_S (param $hm i32) (param $k i32) (param $v i32) (result i32)
    (local $kmv i32)
    (local $res i32)
    (set_local $kmv (call $STRING (get_global $STRING_T) (get_local $k)))
    (set_local $res (call $ASSOC1 (get_local $hm)
                          (get_local $kmv) (get_local $v)))
    ;; map took ownership of key
    (call $RELEASE (get_local $kmv))
    (get_local $res)
  )

  (func $HASHMAP_GET (param $hm i32) (param $key_mv i32) (result i64)
    (local $res i32)
    (local $found i32)
    (local $key i32)
    (local $test_key_mv i32)

    (set_local $key (call $to_String (get_local $key_mv)))
    (set_local $found (i32.const 0))


    (block $done
      (loop $loop
        ;;; if (VAL0(hm) == 0)
        (if (i32.eq (call $VAL0 (get_local $hm)) (i32.const 0))
          (then
            (set_local $res (get_global $NIL))
            (br $done)))
        ;;; test_key_mv = MEM_VAL1(hm)
        (set_local $test_key_mv (call $MEM_VAL1_ptr (get_local $hm)))
        ;;; if (strcmp(key, to_String(test_key_mv)) == 0)
        (if (i32.eq (call $strcmp (get_local $key)
                          (call $to_String (get_local $test_key_mv)))
                    (i32.const 0))
          (then
            (set_local $found (i32.const 1))
            (set_local $res (call $MEM_VAL2_ptr (get_local $hm)))
            (br $done)))
        (set_local $hm (call $MEM_VAL0_ptr (get_local $hm)))

        (br $loop)
      )
    )

    ;; combine found/res as hi 32/low 32 of i64
    (i64.or
      (i64.shl_u (i64.extend_u/i32 (get_local $found))
                 (i64.const 32))
      (i64.extend_u/i32 (get_local $res)))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; function functions

  (func $FUNCTION (param $index i32) (result i32)
    (call $ALLOC_SCALAR (get_global $FUNCTION_T) (get_local $index))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; string functions

  (func $to_String (param $mv i32) (result i32)
    (i32.add (i32.const 4) ;; skip string refcnt
             (call $MalVal_val
                   (call $MalVal_index (get_local $mv))
                   (i32.const 0))))
)
