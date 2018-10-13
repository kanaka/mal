(module $mem
  (global $MEM_SIZE               i32 (i32.const 1048576))
  (global $STRING_MEM_SIZE        i32 (i32.const 1048576))

  (global $heap_start             (mut i32) (i32.const 0))
  (global $heap_end               (mut i32) (i32.const 0))

  (global $mem                    (mut i32) (i32.const 0))
  (global $mem_unused_start       (mut i32) (i32.const 0))
  (global $mem_free_list          (mut i32) (i32.const 0))
  (global $mem_user_start         (mut i32) (i32.const 0))

;;  (global $string_mem             (mut i32) (i32.const 0))
;;  (global $string_mem_next        (mut i32) (i32.const 0))
;;  (global $string_mem_user_start  (mut i32) (i32.const 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General type storage/pointer functions

  (func $VAL0_ptr (param $mv i32) (result i32)
    (i32.add (get_local $mv) (i32.const 4)))
  (func $VAL1_ptr (param $mv i32) (result i32)
    (i32.add (get_local $mv) (i32.const 8)))

  (func $VAL0 (param $mv i32) (result i32)
    (i32.load (i32.add (get_local $mv) (i32.const 4))))
  (func $VAL1 (param $mv i32) (result i32)
    (i32.load (i32.add (get_local $mv) (i32.const 8))))


  (func $MEM_VAL0_ptr (param $mv i32) (result i32)
    (i32.add (get_global $mem)
             (i32.mul_u (i32.load (i32.add (get_local $mv) (i32.const 4)))
                        (i32.const 8))))
  (func $MEM_VAL1_ptr (param $mv i32) (result i32)
    (i32.add (get_global $mem)
             (i32.mul_u (i32.load (i32.add (get_local $mv) (i32.const 8)))
                        (i32.const 8))))
  (func $MEM_VAL2_ptr (param $mv i32) (result i32)
    (i32.add (get_global $mem)
             (i32.mul_u (i32.load (i32.add (get_local $mv) (i32.const 12)))
                        (i32.const 8))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Returns the address of 'mem[mv_idx]'
  (func $MalVal_ptr (param $mv_idx i32) (result i32)
    ;; MalVal memory 64 bit (2 * i32) aligned
    ;;; mem[mv_idx].refcnt_type
    (i32.add (get_global $mem)
             (i32.mul_u (get_local $mv_idx) (i32.const 8))))

  ;; Returns the memory index mem of mv
  ;; Will usually be used with a load or store by the caller
  (func $MalVal_index (param $mv i32) (result i32)
    ;; MalVal memory 64 bit (2 * i32) aligned
    (i32.div_u (i32.sub_u (get_local $mv) (get_global $mem))
               (i32.const 8)))

  ;; Returns the address of 'mem[mv_idx].refcnt_type'
  (func $MalVal_refcnt_type (param $mv_idx i32) (result i32)
    (i32.load (call $MalVal_ptr (get_local $mv_idx))))

  (func $TYPE (param $mv i32) (result i32)
    ;;; type = mv->refcnt_type & 31
    (i32.and (i32.load (get_local $mv))
             (i32.const 0x1f))) ;; 0x1f == 31

  (func $REFS (param $mv i32) (result i32)
    ;;; type = mv->refcnt_type & 31
    (i32.shr_u (i32.load (get_local $mv))
               (i32.const 5))) ;; / 32

  ;; Returns the address of 'mem[mv_idx].val[val]'
  ;; Will usually be used with a load or store by the caller
  (func $MalVal_val_ptr (param $mv_idx i32) (param $val i32) (result i32)
    (i32.add (i32.add (call $MalVal_ptr (get_local $mv_idx))
                      (i32.const 4))
             (i32.mul_u (get_local $val)
                        (i32.const 4))))

  ;; Returns the value of 'mem[mv_idx].val[val]'
  (func $MalVal_val (param $mv_idx i32) (param $val i32) (result i32)
    (i32.load (call $MalVal_val_ptr (get_local $mv_idx) (get_local $val))))

  (func $MalType_size (param $type i32) (result i32)
    ;;; if (type <= 5 || type == 9 || type == 12)
    (if i32 (i32.or (i32.le_u (get_local $type) (i32.const 5))
                    (i32.or (i32.eq (get_local $type) (i32.const 9))
                            (i32.eq (get_local $type) (i32.const 12))))
      (then (i32.const 2))
      (else
        ;;; else if (type == 8 || type == 10 || type == 11)
        (if i32 (i32.or (i32.eq (get_local $type) (i32.const 8))
                        (i32.or (i32.eq (get_local $type) (i32.const 10))
                                (i32.eq (get_local $type) (i32.const 11))))
          (then (i32.const 4))
          (else (i32.const 3))))))

  (func $MalVal_size (param $mv i32) (result i32)
    (local $type i32)
    (set_local $type (call $TYPE (get_local $mv)))
    ;; if (type == FREE_T)
    (if i32 (i32.eq (get_local $type) (get_global $FREE_T))
      (then
        ;;; return (mv->refcnt_type & 0xffe0)>>5
        (i32.shr_u
          (i32.and
            (i32.load (get_local $mv))
            (i32.const 0xffe0))
          (i32.const 5)))  ;;; / 32
      (else
        ;;; return MalType_size(type)
        (call $MalType_size (get_local $type)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; init_memory

  (func $init_memory
    (local $heap_size i32)

;;    (call $print (STRING ">>> init_memory\n"))

    (call $init_sprintf_mem)

    ;; 100 character error_str static buffer
    (set_global $error_str (STRING "                                                                                                    "))
    ;; 256 character token static buffer
    (set_global $token (STRING "                                                                                                                                                                                                                                                                "))

    (set_local $heap_size (i32.add (get_global $MEM_SIZE)
                                   (get_global $STRING_MEM_SIZE)))
    (set_global $heap_start (i32.add (get_global $memoryBase)
                                     (get_global $S_STRING_END)))
    (set_global $heap_end (i32.add (get_global $heap_start)
                                   (get_local $heap_size)))

    (set_global $mem (get_global $heap_start))
    (set_global $mem_unused_start (i32.const 0))
    (set_global $mem_free_list (i32.const 0))

;;    (set_global $string_mem (i32.add (get_global $heap_start)
;;                                     (get_global $MEM_SIZE)))
;;    (set_global $string_mem_next (get_global $string_mem))

    ;; Empty values
    (set_global $NIL
                (call $ALLOC_SCALAR (get_global $NIL_T) (i32.const 0)))
    (set_global $FALSE
                (call $ALLOC_SCALAR (get_global $BOOLEAN_T) (i32.const 0)))
    (set_global $TRUE
                (call $ALLOC_SCALAR (get_global $BOOLEAN_T) (i32.const 1)))
    (set_global $EMPTY_LIST
                (call $ALLOC (get_global $LIST_T)
                      (get_global $NIL) (get_global $NIL) (get_global $NIL)))
    (set_global $EMPTY_VECTOR
                (call $ALLOC (get_global $VECTOR_T)
                      (get_global $NIL) (get_global $NIL) (get_global $NIL)))
    (set_global $EMPTY_HASHMAP
                (call $ALLOC (get_global $HASHMAP_T)
                      (get_global $NIL) (get_global $NIL) (get_global $NIL)))

;;    (call $print (STRING "<<< init_memory\n"))

  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; memory management

  (func $ALLOC_INTERNAL (param $type i32) (param $val1 i32)
               (param $val2 i32) (param $val3 i32) (result i32)
    (local $prev i32)
    (local $res i32)
    (local $size i32)
    (set_local $prev (get_global $mem_free_list))
    (set_local $res (get_global $mem_free_list))
    (set_local $size (call $MalType_size (get_local $type)))

    (block $loop_done
      (loop $loop
        ;; res == mem_unused_start
        (if (i32.eq (get_local $res) (get_global $mem_unused_start))
          (then
            ;; ALLOC_UNUSED
            ;;; if (res + size > MEM_SIZE)
            (if (i32.gt_u (i32.add (get_local $res) (get_local $size))
                          (get_global $MEM_SIZE))
              (then
                ;; Out of memory, exit
                (call $print (STRING "Out of mal memory!\n"))
                (call $exit (i32.const 1))))
            ;;; if (mem_unused_start += size)
            (set_global $mem_unused_start
                        (i32.add (get_global $mem_unused_start)
                                 (get_local $size)))
            ;;; if (prev == res)
            (if (i32.eq (get_local $prev) (get_local $res))
              (then
                (set_global $mem_free_list (get_global $mem_unused_start)))
              (else
                ;;; mem[prev].val[0] = mem_unused_start
                (i32.store
                  (call $MalVal_val_ptr (get_local $prev) (i32.const 0))
                  (get_global $mem_unused_start))))
            (br $loop_done)))
        ;; if (MalVal_size(mem+res) == size)
        (if (i32.eq (call $MalVal_size (call $MalVal_ptr (get_local $res)))
                    (get_local $size))
          (then
            ;; ALLOC_MIDDLE
            ;;; if (res == mem_free_list)
            (if (i32.eq (get_local $res) (get_global $mem_free_list))
              ;; set free pointer (mem_free_list) to next free
              ;;; mem_free_list = mem[res].val[0];
              (set_global $mem_free_list
                          (call $MalVal_val (get_local $res) (i32.const 0))))
            ;;  if (res != mem_free_list)
            (if (i32.ne (get_local $res) (get_global $mem_free_list))
              ;; set previous free to next free
              ;;; mem[prev].val[0] = mem[res].val[0]
              (i32.store (call $MalVal_val_ptr (get_local $prev) (i32.const 0))
                         (call $MalVal_val (get_local $res) (i32.const 0))))
            (br $loop_done)))
        ;;; prev = res
        (set_local $prev (get_local $res))
        ;;; res = mem[res].val[0]
        (set_local $res (call $MalVal_val (get_local $res) (i32.const 0)))
        (br $loop)
      )
    )
    ;; ALLOC_DONE
    ;;; mem[res].refcnt_type = type + 32
    (i32.store (call $MalVal_ptr (get_local $res))
               (i32.add (get_local $type) (i32.const 32)))
    ;; set val to default val1
    ;;; mem[res].val[0] = val1
    (i32.store (call $MalVal_val_ptr (get_local $res) (i32.const 0))
               (get_local $val1))
    ;;; if (type > 5 && type != 9)
    (if (i32.and (i32.gt_u (get_local $type) (i32.const 5))
                 (i32.ne (get_local $type) (i32.const 9)))
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val1].refcnt_type += 32
        (i32.store (call $MalVal_ptr (get_local $val1))
                   (i32.add (call $MalVal_refcnt_type (get_local $val1))
                            (i32.const 32)))))
    ;;; if (size > 2)
    (if (i32.gt_u (get_local $size) (i32.const 2))
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val2].refcnt_type += 32
        (i32.store (call $MalVal_ptr (get_local $val2))
                   (i32.add (call $MalVal_refcnt_type (get_local $val2))
                            (i32.const 32)))
        ;;; mem[res].val[1] = val2
        (i32.store (call $MalVal_val_ptr (get_local $res) (i32.const 1))
                   (get_local $val2))))
    ;;; if (size > 3)
    (if (i32.gt_u (get_local $size) (i32.const 3))
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val3].refcnt_type += 32
        (i32.store (call $MalVal_ptr (get_local $val3))
                   (i32.add (call $MalVal_refcnt_type (get_local $val3))
                            (i32.const 32)))
        ;;; mem[res].val[2] = val3
        (i32.store (call $MalVal_val_ptr (get_local $res) (i32.const 2))
                   (get_local $val3))))

    ;;; return mem + res
    (call $MalVal_ptr (get_local $res))
  )

  (func $ALLOC_SCALAR (param $type i32) (param $val1 i32)
                      (result i32)
    (call $ALLOC_INTERNAL
          (get_local $type)
          (get_local $val1)
          (i32.const 0)
          (i32.const 0))
  )

  (func $ALLOC (param $type i32) (param $val1 i32)
               (param $val2 i32) (param $val3 i32) (result i32)
    (call $ALLOC_INTERNAL
          (get_local $type)
          (call $MalVal_index (get_local $val1))
          (call $MalVal_index (get_local $val2))
          (call $MalVal_index (get_local $val3)))
  )

  (func $RELEASE (param $mv i32)
    (local $idx i32)
    (local $type i32)
    (local $size i32)

    ;; Ignore NULLs
    ;;; if (mv == NULL) { return; }
    (if (i32.eqz (get_local $mv)) (return))
    ;;; idx = mv - mem
    (set_local $idx (call $MalVal_index (get_local $mv)))
    ;;; type = mv->refcnt_type & 31
    (set_local $type (i32.and (i32.load (get_local $mv))
                              (i32.const 0x1f))) ;; 0x1f == 31
    ;;; size = MalType_size(type)
    (set_local $size (call $MalType_size (get_local $type)))

    ;; DEBUG
    ;;; printf(">>> RELEASE idx: %d, type: %d, size: %d\n", idx, type, size)
;;    (call $print (STRING ">>> RELEASE idx: "))
;;    (call $printhex (get_local $idx))
;;    (call $print (STRING ", type: "))
;;    (call $printnum (get_local $type) (i32.const 10))
;;    (call $print (STRING ", size: "))
;;    (call $printnum (get_local $size) (i32.const 10))
;;    (call $print (STRING "\n"))

    (if (i32.eq (i32.const 0) (get_local $mv))
      (then
        (call $print (STRING "RELEASE of NULL!\n"))
        (call $exit (i32.const 1))))

    (if (i32.eq (get_global $FREE_T) (get_local $type))
      (then
        (call $printf_2 (STRING "RELEASE of already free mv: 0x%x, idx: 0x%x\n")
              (get_local $mv) (get_local $idx))
        (call $exit (i32.const 1))))
    (if (i32.lt_u (call $MalVal_refcnt_type (get_local $idx))
                  (i32.const 15))
      (then
        (call $printf_2 (STRING "RELEASE of unowned mv: 0x%x, idx: 0x%x\n")
              (get_local $mv) (get_local $idx))
        (call $exit (i32.const 1))))

    ;; decrease reference count by one
    (i32.store (call $MalVal_ptr (get_local $idx))
               (i32.sub_u (call $MalVal_refcnt_type (get_local $idx))
                          (i32.const 32)))

    ;; nil, false, true, empty sequences
    (if (i32.le_u (get_local $mv) (get_global $EMPTY_HASHMAP))
      (then
        (if (i32.lt_u (call $MalVal_refcnt_type (get_local $idx))
                      (i32.const 32))
          (then
            (call $printf_2 (STRING "RELEASE of unowned mv: 0x%x, idx: 0x%x\n")
                  (get_local $mv) (get_local $idx))
            (call $exit (i32.const 1))))
        (return)))

    ;; our reference count is not 0, so don't release
    (if (i32.ge_u (call $MalVal_refcnt_type (get_local $idx))
                  (i32.const 32))
      (return))

    (block $done
      (block (block (block (block (block (block
      (br_table 0 0 0 0 1 1 2 2 3 5 5 5 5 4 5 5 5 (get_local $type)))
      ;; nil, boolean, integer, float
      (br $done))
      ;; string, kw, symbol
      ;; release string, then FREE reference
      (call $RELEASE_STRING (get_local $mv))
      (br $done))
      ;; list, vector
      (if (i32.ne (call $MalVal_val (get_local $idx) (i32.const 0))
                  (i32.const 0))
        (then
          ;; release next element and value
          (call $RELEASE (call $MEM_VAL0_ptr (get_local $mv)))
          (call $RELEASE (call $MEM_VAL1_ptr (get_local $mv)))))
      (br $done))
      ;; hashmap
      (if (i32.ne (call $MalVal_val (get_local $idx) (i32.const 0))
                  (i32.const 0))
        (then
          ;; release next element, value, and key
          (call $RELEASE (call $MEM_VAL0_ptr (get_local $mv)))
          (call $RELEASE (call $MEM_VAL2_ptr (get_local $mv)))
          (call $RELEASE (call $MEM_VAL1_ptr (get_local $mv)))))
      (br $done))
      ;; env
      ;; if outer is set then release outer
      (if (i32.ne (call $MalVal_val (get_local $idx) (i32.const 1))
                  (i32.const 0))
        (call $RELEASE (call $MEM_VAL1_ptr (get_local $mv))))
      ;; release the hashmap data
      (call $RELEASE (call $MEM_VAL0_ptr (get_local $mv)))
      (br $done))
      ;; default/unknown
    )

    ;; FREE, free the current element

    ;; set type(FREE/15) and size
    ;;; mv->refcnt_type = size*32 + FREE_T
    (i32.store (get_local $mv)
               (i32.add (i32.mul_u (get_local $size)
                                   (i32.const 32))
                        (get_global $FREE_T)))
    (i32.store (call $MalVal_val_ptr (get_local $idx) (i32.const 0))
               (get_global $mem_free_list))
    (set_global $mem_free_list (get_local $idx))
    (if (i32.ge_u (get_local $size) (i32.const 3))
      (i32.store (call $MalVal_val_ptr (get_local $idx) (i32.const 1))
                 (i32.const 0)))
    (if (i32.eq (get_local $size) (i32.const 4))
      (i32.store (call $MalVal_val_ptr (get_local $idx) (i32.const 2))
                 (i32.const 0)))
  )

  ;; Allocate a string as follows:
  ;; refcnt (i32 set to 1), string data, NULL byte
  (func $STRING_DUPE (param $str i32) (result i32)
    (local $len i32)
    (local $cur i32)
    (local $new i32)
    (local $idx i32)

    ;; Calculate length of string needed
    (set_local $len (call $STRING_LEN (get_local $str)))

    ;; leading i32 refcnt + trailing NULL
    (set_local $new (call $malloc (i32.add (i32.const 5) (get_local $len))))

    ;; DEBUG
;;    (call $debug (STRING "STRING_DUPE - malloc returned: ") (get_local $new))

    ;; set initial refcnt to 1
    (i32.store (get_local $new) (i32.const 1))
    ;; skip refcnt
    (set_local $cur (i32.add (get_local $new) (i32.const 4)))
    ;; Set NULL terminator
    (i32.store8_u (i32.add (get_local $cur) (get_local $len)) (i32.const 0))

    ;; Copy the characters
    (call $MEM_COPY (get_local $cur) (get_local $str) (get_local $len))
    (get_local $new)
  )

  ;; Duplicate regular character array string into a Mal string and
  ;; return the MalVal pointer
  (func $STRING (param $type i32) (param $str i32) (result i32)
    (call $ALLOC_SCALAR
          (get_local $type)
          (call $STRING_DUPE (get_local $str)))
  )

  (func $RELEASE_STRING (param $mv i32)
    (local $str i32)
    (set_local $str (call $MalVal_val
                          (call $MalVal_index (get_local $mv))
                          (i32.const 0)))

    ;; DEBUG
;;    (call $debug (STRING "RELEASE_STRING - calling free on: ") (get_local $str))

    (call $free (get_local $str))
  )
)
