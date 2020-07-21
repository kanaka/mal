;; Core functions
;;
;;

%include "macros.mac"
        
section .data

;; Symbols for comparison
        static core_add_symbol, db "+"
        static core_sub_symbol, db "-"
        static core_mul_symbol, db "*"
        static core_div_symbol, db "/"

        static core_listp_symbol, db "list?"
        static core_emptyp_symbol, db "empty?"
        
        static core_equal_symbol, db "="
        static core_gt_symbol, db ">"
        static core_lt_symbol, db "<"
        static core_ge_symbol, db ">="
        static core_le_symbol, db "<="

        static core_count_symbol, db "count"
        static core_keys_symbol, db "keys"
        static core_vals_symbol, db "vals"

        static core_list_symbol, db "list"

        static core_pr_str_symbol, db "pr-str"
        static core_prn_symbol, db "prn"
        static core_str_symbol, db "str"
        static core_println_symbol, db "println"

        static core_read_string_symbol, db "read-string"
        static core_slurp_symbol, db "slurp"
        static core_eval_symbol, db "eval"

        static core_atom_symbol, db "atom"
        static core_deref_symbol, db "deref"
        static core_atomp_symbol, db "atom?"
        static core_reset_symbol, db "reset!"
        static core_swap_symbol, db "swap!"

        static core_cons_symbol, db "cons"
        static core_concat_symbol, db "concat"
        static core_vec_symbol, db "vec"

        static core_first_symbol, db "first"
        static core_rest_symbol, db "rest"
        static core_nth_symbol, db "nth"

        static core_nilp_symbol, db "nil?"
        static core_truep_symbol, db "true?"
        static core_falsep_symbol, db "false?"
        static core_numberp_symbol, db "number?"

        static core_symbolp_symbol, db "symbol?"
        static core_stringp_symbol, db "string?"
        static core_fnp_symbol, db "fn?"
        static core_macrop_symbol, db "macro?"
        static core_keywordp_symbol, db "keyword?"

        static core_containsp_symbol, db "contains?"
        static core_get_symbol, db "get"
        static core_vectorp_symbol, db "vector?"
        static core_mapp_symbol, db "map?"
        static core_sequentialp_symbol, db "sequential?"
        
        static core_throw_symbol, db "throw"
        
        static core_map_symbol, db "map"
        static core_apply_symbol, db "apply"

        static core_symbol_symbol, db "symbol"
        static core_vector_symbol, db "vector"
        static core_hashmap_symbol, db "hash-map"
        static core_keyword_symbol, db "keyword"

        static core_assoc_symbol, db "assoc"
        static core_dissoc_symbol, db "dissoc"

        static core_readline_symbol, db "readline"

        static core_meta_symbol, db "meta"
        static core_with_meta_symbol, db "with-meta"

        static core_time_ms_symbol, db "time-ms"

        static core_seq_symbol, db "seq"
        
;; Strings

        static core_arith_missing_args, db "integer arithmetic missing arguments"
        static core_arith_not_int, db "non-integer argument to integer arithmetic"
        
        static core_emptyp_error_string, db "empty? expects a list, vector or map",10
        static core_count_error_string, db "count expects a list or vector",10

        static core_keys_not_map, db "keys expects a map as first argument"
        static core_vals_not_map, db "vals expects a map as first argument"
        
        static core_numeric_expect_ints, db "comparison operator expected two numbers",10

        static core_deref_not_atom, db "Error: argument to deref is not an atom"
        static core_reset_not_atom, db "Error: argument to reset is not an atom"
        static core_reset_no_value, db "Error: missing value argument to reset"

        static core_swap_not_atom, db "Error: swap! expects atom as first argument"
        static core_swap_no_function, db "Error: swap! expects function as second argument"
        
        static core_cons_missing_arg, db "Error: missing argument to cons"
        static core_cons_not_vector, db "Error: cons expects a list or vector"
        
        static core_concat_not_list, db "Error: concat expects lists or vectors"

        static core_vec_wrong_arg, db "Error: vec expects a list or vector "

        static core_first_missing_arg, db "Error: missing argument to first"
        static core_first_not_list, db "Error: first expects a list or vector"
        
        static core_rest_missing_arg, db "Error: missing argument to rest"
        static core_rest_not_list, db "Error: rest expects a list or vector"

        static core_nth_missing_arg, db "Error: missing argument to nth"
        static core_nth_not_list, db "Error: nth expects a list or vector as first argument"
        static core_nth_not_int, db "Error: nth expects an integer as second argument"
        static core_nth_out_of_range, db "Error: nth index out of range"

        static core_value_p_missing_args, db "Error: value predicate (nil/true/false) missing args"

        static core_containsp_not_map, db "Error: contains? expects map as first argument"
        static core_containsp_no_key, db "Error: contains? missing key argument"

        static core_get_not_map, db "Error: get expects map as first argument"
        static core_get_no_key, db "Error: get missing key argument"

        static core_map_missing_args, db "Error: map expects two arguments (function, list/vector)"
        static core_map_not_function, db "Error: map expects a ufunction for first argument"
        static core_map_not_seq, db "Error: map expects a list or vector as second argument"

        static core_apply_not_function, db "Error: apply expects function as first argument"
        static core_apply_missing_args, db "Error: apply missing arguments"
        static core_apply_not_seq, db "Error: apply last argument must be list or vector"

        static core_symbol_not_string, db "Error: symbol expects a string argument"

        static core_keyword_not_string, db "Error: keyword expects a string argument"

        static core_list_not_seq, db "Error: list expects a list or vector"

        static core_assoc_not_map, db "Error: assoc expects a map as first argument"
        static core_assoc_missing_value, db "Error: assoc missing value"

        static core_dissoc_not_map, db "dissoc expects a map as first argument"
        static core_dissoc_missing_value, db "Missing value in map passed to dissoc"

        static core_with_meta_no_function, db "with-meta expects a function as first argument"
        static core_with_meta_no_value, db "with-meta expects a value as second argument"

        static core_seq_missing_arg, db "seq missing argument"
        static core_seq_wrong_type, db "seq expects a list, vector, string or nil"
        
section .text

;; Add a native function to the core environment
;; This is used in core_environment
%macro core_env_native 2
        push rsi                ; environment
        mov rsi, %1
        mov edx, %1.len
        call raw_to_symbol      ; Symbol in RAX
        push rax
        
        mov rsi, %2
        call native_function    ; Function in RAX
        
        mov rcx, rax            ; value (function)
        pop rdi                 ; key (symbol)
        pop rsi                 ; environment
        call env_set
%endmacro

        
;; Create an Environment with core functions
;;
;; Returns Environment in RAX
;;
;;
core_environment:
        ; Create the top-level environment
        xor rsi, rsi            ; Set outer to nil
        call env_new            
        mov rsi, rax            ; Environment in RSI
        
        core_env_native core_cons_symbol, core_cons
        core_env_native core_concat_symbol, core_concat
        core_env_native core_vec_symbol, core_vec
        
        core_env_native core_first_symbol, core_first
        core_env_native core_rest_symbol, core_rest
        core_env_native core_nth_symbol, core_nth

        core_env_native core_add_symbol, core_add
        core_env_native core_sub_symbol, core_sub
        core_env_native core_mul_symbol, core_mul
        core_env_native core_div_symbol, core_div
        
        core_env_native core_listp_symbol, core_listp
        core_env_native core_emptyp_symbol, core_emptyp
        core_env_native core_count_symbol, core_count
        
        core_env_native core_equal_symbol, core_equalp
        core_env_native core_gt_symbol, core_gt
        core_env_native core_lt_symbol, core_lt
        core_env_native core_ge_symbol, core_ge
        core_env_native core_le_symbol, core_le
        
        core_env_native core_keys_symbol, core_keys
        core_env_native core_vals_symbol, core_vals
        
        core_env_native core_list_symbol, core_list

        core_env_native core_pr_str_symbol, core_pr_str
        core_env_native core_prn_symbol, core_prn
        core_env_native core_str_symbol, core_str
        core_env_native core_println_symbol, core_println

        core_env_native core_read_string_symbol, core_read_string
        core_env_native core_slurp_symbol, core_slurp
        core_env_native core_eval_symbol, core_eval

        core_env_native core_atom_symbol, core_atom
        core_env_native core_deref_symbol, core_deref
        core_env_native core_atomp_symbol, core_atomp
        core_env_native core_reset_symbol, core_reset
        core_env_native core_swap_symbol, core_swap
        
        core_env_native core_nilp_symbol, core_nilp
        core_env_native core_truep_symbol, core_truep
        core_env_native core_falsep_symbol, core_falsep
        core_env_native core_numberp_symbol, core_numberp

        core_env_native core_symbolp_symbol, core_symbolp
        core_env_native core_stringp_symbol, core_stringp
        core_env_native core_fnp_symbol, core_fnp
        core_env_native core_macrop_symbol, core_macrop
        core_env_native core_keywordp_symbol, core_keywordp
        
        core_env_native core_containsp_symbol, core_containsp
        core_env_native core_get_symbol, core_get
        
        core_env_native core_vectorp_symbol, core_vectorp
        core_env_native core_mapp_symbol, core_mapp
        core_env_native core_sequentialp_symbol, core_sequentialp
        
        core_env_native core_throw_symbol, core_throw

        core_env_native core_map_symbol, core_map
        core_env_native core_apply_symbol, core_apply

        core_env_native core_symbol_symbol, core_symbol
        core_env_native core_vector_symbol, core_vector
        core_env_native core_hashmap_symbol, core_hashmap
        core_env_native core_keyword_symbol, core_keyword

        core_env_native core_assoc_symbol, core_assoc
        core_env_native core_dissoc_symbol, core_dissoc

        core_env_native core_readline_symbol, core_readline

        core_env_native core_meta_symbol, core_meta
        core_env_native core_with_meta_symbol, core_with_meta

        core_env_native core_time_ms_symbol, core_time_ms

        core_env_native core_seq_symbol, core_seq
        
        ; -----------------
        ; Put the environment in RAX
        mov rax, rsi
        ret

;; ----------------------------------------------------

;; Jumped to from many core functions, with
;; string address in RSI and length in EDX
core_throw_str:
        call raw_to_string
        mov rsi, rax
        jmp error_throw

;; ----------------------------------------------------

     
        
;; Integer arithmetic operations
;; 
;; Adds a list of numbers, address in RSI
;; Returns the sum as a number object with address in RAX
;; Since most of the code is common to all operators,
;; RBX is used to jump to the required instruction
core_add:
        mov rbx, core_arithmetic.do_addition
        jmp core_arithmetic
core_sub:
        mov rbx, core_arithmetic.do_subtraction
        jmp core_arithmetic
core_mul:
        mov rbx, core_arithmetic.do_multiply
        jmp core_arithmetic
core_div:
        mov rbx, core_arithmetic.do_division
        ; Fall through to core_arithmetic
core_arithmetic:
        ; Check that the first object is a number
        mov cl, BYTE [rsi]
        mov ch, cl
        and ch, block_mask
        cmp ch, block_cons
        jne .missing_args

        mov ch, cl
        and ch, content_mask
        cmp ch, content_empty
        je .missing_args

        cmp ch, content_int
        jne .not_int

        ; Put the starting value in rax
        mov rax, [rsi + Cons.car]
        
.add_loop:
        ; Fetch the next value
        mov cl, [rsi + Cons.typecdr]
        cmp cl, content_pointer
        jne .finished  ; Nothing let
        
        mov rsi, [rsi + Cons.cdr] ; Get next cons

        ; Check that it is an integer
        mov cl, BYTE [rsi]
        and cl, content_mask
        cmp cl, content_int
        jne .not_int

        ; Jump to the required operation, address in RBX
        jmp rbx
        
.do_addition:
        add rax, [rsi + Cons.car]
        jmp .add_loop
.do_subtraction:
        sub rax, [rsi + Cons.car]
        jmp .add_loop
.do_multiply:
        imul rax, [rsi + Cons.car]
        jmp .add_loop
.do_division:
        cqo                     ; Sign extend RAX into RDX
        mov rcx, [rsi + Cons.car]
        idiv rcx
        jmp .add_loop
        
.finished:
        ; Value in rbx
        push rax
        ; Get a Cons object to put the result into
        call alloc_cons
        pop rbx
        mov [rax], BYTE maltype_integer
        mov [rax + Cons.car], rbx
        ret
        
.missing_args:
        load_static core_arith_missing_args
        jmp core_throw_str
.not_int:
        load_static core_arith_not_int
        jmp core_throw_str

;; compare objects for equality
core_equalp:
        ; Check that rsi contains a list
        mov cl, BYTE [rsi]
        cmp cl, maltype_empty_list
        je .error
        
        and cl, block_mask + container_mask
        cmp cl, block_cons + container_list
        jne .error
        
        ; Check that the list has a second pointer
        mov cl, BYTE [rsi + Cons.typecdr]
        cmp cl, content_pointer
        jne .error
        
        ; move second pointer into rdi
        mov rdi, [rsi + Cons.cdr]

        ; Remove next pointers
        mov cl, BYTE [rsi + Cons.typecdr]
        mov [rsi + Cons.typecdr], BYTE 0
        
        mov bl, BYTE [rdi + Cons.typecdr]
        mov [rdi + Cons.typecdr], BYTE 0

        push rbx
        push rcx
        
        ; Compare the objects recursively
        call compare_objects_rec

        ; Restore next pointers
        pop rcx
        pop rbx
        mov [rsi + Cons.typecdr], BYTE cl
        mov [rdi + Cons.typecdr], BYTE bl
        
        je .true

        
.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret
.true:
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
.error:
        push rsi
        print_str_mac error_string ; print 'Error: '
        pop rsi
        jmp error_throw

;; -----------------------------------------------------------------
;; Numerical comparisons


core_gt:
        mov rcx, core_compare_num.gt
        jmp core_compare_num
core_lt:
        mov rcx, core_compare_num.lt
        jmp core_compare_num
core_ge:
        mov rcx, core_compare_num.ge
        jmp core_compare_num
core_le:
        mov rcx, core_compare_num.le  
        ;jmp core_compare_num
core_compare_num:
        ; The first argument should be an int
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, maltype_integer
        jne .error

        ; Check that there's a second argument
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .error
        mov rax, [rsi + Cons.car]
        mov rdi, [rsi + Cons.cdr]

        ; The second arg should also be an int
        mov bl, BYTE [rdi]
        and bl, content_mask
        cmp bl, maltype_integer
        jne .error

        mov rbx, [rdi + Cons.car]

        cmp rax, rbx
        jmp rcx                 ; Address set above
.gt:
        jg .true
        jmp .false
.lt:
        jl .true
        jmp .false
.ge:
        jge .true
        jmp .false
.le:
        jle .true
        ;jmp .false
.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret
.true:
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
.error:
        push rsi
        print_str_mac error_string ; print 'Error: '
        print_str_mac core_numeric_expect_ints
        pop rsi
        jmp error_throw
        
;; Test if a given object is a list
;; Input list in RSI
;; Returns true or false in RAX
core_listp:
        mov bl, (block_cons + container_list)
        jmp core_container_p
core_vectorp:
        mov bl, (block_cons + container_vector)
        jmp core_container_p
core_mapp:
        mov bl, (block_cons + container_map)
        ;jmp core_container_p     
core_container_p:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .false              ; Should be a pointer to a list

        mov rax, [rsi + Cons.car]
        mov al, BYTE [rax]
        and al, (block_mask + container_mask)
        cmp al, bl
        jne .false

        ; Is a list, return true
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
        
.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret

;; Return true if vector or list
core_sequentialp:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .false              ; Should be a pointer

        mov rax, [rsi + Cons.car]
        mov al, BYTE [rax]
        and al, (block_mask + container_mask)
        cmp al, container_list
        je .true
        cmp al, container_vector
        jne .false
.true:
        ; Is a list or vector, return true
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
        
.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret


        
;; Test if the given list, vector or map is empty
core_emptyp:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .error              ; Expected a container
        mov rax, [rsi + Cons.car]
        mov al, BYTE [rax]
        cmp al, maltype_empty_list
        je .true
        cmp al, maltype_empty_vector
        je .true
        cmp al, maltype_empty_map
        je .true

        ; false
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret
.true:
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
.error:
        push rsi
        print_str_mac error_string
        print_str_mac core_emptyp_error_string
        pop rsi
        jmp error_throw

;; Count the number of elements in given list or vector
core_count:
        mov al, BYTE [rsi]
        and al, content_mask
        
        cmp al, content_nil
        je .zero
        
        cmp al, content_pointer
        jne .error              ; Expected a container

        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        
        mov ah, al
        and ah, (block_mask + container_mask)
        cmp ah, (block_cons + container_list)
        je .start_count
        cmp ah, (block_cons + container_vector)
        je .start_count
        
        jmp .error              ; Not a list or vector
        
.start_count:
        
        xor rbx,rbx
        mov ah, al
        and ah, content_mask
        cmp ah, content_empty
        je .done                ; Empty list or vector

.loop:
        inc rbx

        ; Check if there's another
        mov al, [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done

        mov rsi, [rsi + Cons.cdr]
        jmp .loop

.zero:                          ; Return zero count
        mov rbx, 0
.done:                          ; Count is in RBX

        push rbx
        call alloc_cons
        pop rbx
        mov [rax], BYTE maltype_integer
        mov [rax + Cons.car], rbx
        ret
        
.error:
        push rsi
        print_str_mac error_string
        print_str_mac core_count_error_string
        pop rsi
        jmp error_throw
        
        
;; Given a map, returns a list of keys
;; Input: List in RSI with one Map element
;; Returns: List in RAX
core_keys:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_map
        
        mov rsi, [rsi + Cons.car]
        call map_keys
        ret
.not_map:
        load_static core_keys_not_map
        jmp core_throw_str
        
;; Get a list of values from a map
core_vals:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_map
        
        mov rsi, [rsi + Cons.car]
        call map_vals
        ret
.not_map:
        load_static core_vals_not_map
        jmp core_throw_str
        
;; Given a map and a key, return true if the key is in the map
;;
core_containsp:
        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        and bl, content_mask
        cmp bl, content_pointer
        jne .not_map

        mov rcx, [rsi + Cons.car]  ; Map in RCX
        mov bl, BYTE [rcx]
        and bl, (block_mask + container_mask)
        cmp bl, container_map
        jne .not_map
        
        ; Check second argument
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_key
        mov rsi, [rsi + Cons.cdr]
        mov dl, BYTE [rsi]
        and dl, content_mask
        cmp dl, content_pointer
        jne .key_value

        ; Pointer, so put into RDI
        mov rdi, [rsi + Cons.car]
        jmp .find
        
.key_value:
        ; A value
        mov [rsi], BYTE dl
        mov rdi, rsi            ; Value in RDI
        
.find:
        mov rsi, rcx            ; Map
        call map_find
        je .true

        ; false
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret
.true:
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
        
.not_map:
        load_static core_containsp_not_map
        jmp core_throw_str
.no_key:
        load_static core_containsp_no_key
        jmp core_throw_str


;; Given a map and a key, return the value in the map
;; or nil if not found
;;
core_get:
        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        
        and bl, content_mask
        
        cmp bl, content_nil
        je .not_found
        
        cmp bl, content_pointer
        jne .not_map
        
        mov rcx, [rsi + Cons.car]  ; Map in RCX
        mov bl, BYTE [rcx]
        and bl, (block_mask + container_mask)
        cmp bl, container_map
        jne .not_map
        
        ; Check second argument
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_key
        mov rsi, [rsi + Cons.cdr]
        
        mov dl, BYTE [rsi]
        and dl, content_mask
        cmp dl, content_pointer
        jne .key_value

        ; Pointer, so put into RDI
        mov rdi, [rsi + Cons.car]
        jmp .find
        
.key_value:
        ; A value
        mov [rsi], BYTE dl
        mov rdi, rsi            ; Value in RDI
        
.find:
        mov rsi, rcx            ; Map
        call map_get            ; Value in RAX
        je .found

.not_found:
        ; Not found
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
.found:
        ret
        
.not_map:
        load_static core_get_not_map
        jmp core_throw_str
.no_key:
        load_static core_get_no_key
        jmp core_throw_str

        
;; Return arguments as a list
;; 
core_list:
        call incref_object
        mov rax, rsi
        ret
        
;; Convert arguments into a vector
core_vector:
        ; Copy first element and mark as vector
        call alloc_cons         ; in RAX
        mov bl, BYTE [rsi]
        and bl, content_mask
        mov bh, bl              ; store content for comparison
        or bl, container_vector
        mov [rax], BYTE bl      ; Set type
        
        mov rcx, [rsi + Cons.car]
        mov [rax + Cons.car], rcx ; Set content

        ; Check if the first element is a pointer
        cmp bh, content_pointer
        jne .done_car
        
        ; A pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx

.done_car:
        ; Copy the CDR type and content
        mov bl, [rsi + Cons.typecdr]
        mov [rax + Cons.typecdr], bl

        mov rdx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rdx
        
        cmp bl, content_pointer
        jne .done

        ; A pointer
        mov bx, WORD [rdx + Cons.refcount]
        inc bx
        mov [rdx + Cons.refcount], WORD bx
        
.done:
        ret

        
;; Convert arguments into a map
core_hashmap:
        ; Copy first element and mark as map
        call alloc_cons         ; in RAX
        mov bl, BYTE [rsi]
        and bl, content_mask
        mov bh, bl              ; store content for comparison
        or bl, container_map
        mov [rax], BYTE bl      ; Set type
        
        mov rcx, [rsi + Cons.car]
        mov [rax + Cons.car], rcx ; Set content

        ; Check if the first element is a pointer
        cmp bh, content_pointer
        jne .done_car
        
        ; A pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx

.done_car:
        ; Copy the CDR type and content
        mov bl, [rsi + Cons.typecdr]
        mov [rax + Cons.typecdr], bl

        mov rdx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rdx
        
        cmp bl, content_pointer
        jne .done

        ; A pointer
        mov bx, WORD [rdx + Cons.refcount]
        inc bx
        mov [rdx + Cons.refcount], WORD bx
        
.done:
        ret
        
;; ------------------------------------------------
;; String functions

;; Convert arguments to a readable string, separated by a space
;; 
core_pr_str:
        mov rdi, 3              ; print_readably & separator
        jmp core_str_functions
core_str:
        xor rdi, rdi
        jmp core_str_functions
core_str_sep:
        mov rdi, 2              ; separator
        
core_str_functions:
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        cmp ah, content_empty
        je .empty              ; Nothing to print

        xor r8, r8              ; Return string in r8
        
.loop:
        cmp ah, content_pointer
        je .got_pointer
        
        ; A value. Remove list container
        xchg ah, al
        mov [rsi], BYTE al
        xchg ah, al
        push rsi
        push rax
        push r8
        call pr_str
        pop r8
        pop rbx
        pop rsi
        mov [rsi], BYTE bl      ; restore type
        jmp .got_string
        
.got_pointer:
        push rsi
        push r8
        mov rsi, [rsi + Cons.car] ; Address pointed to
        call pr_str
        pop r8
        pop rsi
        
.got_string:
        ; String now in rax
        
        cmp r8, 0
        jne .append

        ; first string. Since this string will be
        ; appended to, it needs to be a copy
        push rsi                ; input
        
        push rax                ; string to copy
        mov rsi, rax
        call string_copy        ; New string in RAX
        pop rsi                 ; copied string
        
        push rax                ; the copy
        call release_object     ; release the copied string
        pop r8                  ; the copy

        pop rsi                 ; input
        
        jmp .next
        
.append:
        push r8
        push rsi
        push rax
        
        mov rsi, r8             ; Output string 
        mov rdx, rax            ; String to be copied
        call string_append_string
        
        pop rsi                 ; Was in rax, temporary string
        call release_array      ; Release the string

        pop rsi                 ; Restore input
        pop r8                  ; Output string
.next:
        ; Check if there's another
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done

        ; More inputs
        mov rsi, [rsi + Cons.cdr] ; pointer

        test rdi, 2             ; print_readably
        jz .end_append_char     ; No separator
        
        ; Add separator
        push r8
        push rsi
        mov rsi, r8
        mov cl, ' '
        call string_append_char
        pop rsi
        pop r8
.end_append_char:
        
        ; Get the type in ah for comparison at start of loop
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        
        jmp .loop
.done:
        ; No more input, so return
        mov rax, r8
        ret
        
.empty:
        call string_new         ; An empty string
        ret
        
;; Print arguments readably, return nil
core_prn:
        call core_pr_str
        jmp core_prn_functions
core_println:
        call core_str_sep
core_prn_functions:
        mov rsi, rax

        ; Put newline at the end
        push rsi
        mov cl, 10              ; newline
        call string_append_char
        pop rsi
        
        ; print the string
        push rsi                ; Save the string address
        call print_string
        pop rsi
        call release_array      ; Release the string

        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret

;; Given a string, calls read_str to get an AST
core_read_string:
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        jne .no_string
        
        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        cmp al, maltype_string
        jne .no_string
        
        call read_str
        ret
        
.no_string:
        ; Didn't get a string input
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        

;; Reads a file into a string
core_slurp:
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        jne .no_string
        
        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        cmp al, maltype_string
        jne .no_string

        call read_file
        ret
        
.no_string:
        ; Didn't get a string input
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret

;; Evaluate an expression in the REPL environment
;;
core_eval:
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        je .pointer

        ; Just a value, so return it
        call incref_object
        
        mov al, BYTE [rsi]
        and al, content_mask
        mov [rsi], BYTE al      ; Removes list
        mov rax, rsi
        ret
        
.pointer:
        ; A pointer, so need to eval
        mov rdi, [rsi + Cons.car]
        
        mov rsi, [repl_env]     ; Environment
        
        call incref_object      ; Environment increment refs
        xchg rsi, rdi           ; Env in RDI, AST in RSI

        call incref_object      ; AST increment refs
        
        call eval
        ret

;; Create an atom
core_atom:
        push rsi
        call alloc_cons         ; To hold the pointer
        pop rsi
        mov [rax], BYTE maltype_atom

        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        je .pointer

        ; A value
        
        ; make a copy
        push rax
        push rsi
        push rbx
        call alloc_cons
        pop rbx

        mov bl, bh
        mov [rax], BYTE bl      ; Set type
        
        mov rbx, rax
        pop rsi
        pop rax
        
        mov rcx, [rsi + Cons.car]
        mov [rbx + Cons.car], rcx ; Set value
        
        ; Set the atom to point to it
        mov [rax + Cons.car], rbx
        
        ret
        
.pointer:
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx

        push rax
        mov rsi, rbx
        call incref_object      ; Storing in atom
        pop rax
        ret

;; Get the value from the atom
core_deref:
        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        jne .not_atom

        ; Get the atom
        mov rsi, [rsi + Cons.car]
        mov bl, BYTE [rsi]
        cmp bl, maltype_atom
        jne .not_atom

        ; Return what it points to
        mov rsi, [rsi + Cons.car]
        call incref_object
        mov rax, rsi
        ret
        
.not_atom:
        ; Not an atom, so throw an error
        mov rsi, core_deref_not_atom
        mov edx, core_deref_not_atom.len
        call raw_to_symbol
        mov rsi, rax
        jmp error_throw

;; Test if given object is an atom
core_atomp:
        mov al, maltype_atom
        jmp core_pointer_type_p
core_symbolp:
        mov al, maltype_symbol
        jmp core_pointer_type_p
core_stringp:
        mov al, maltype_string
        jmp core_pointer_type_p
core_fnp:
        mov al, maltype_function
        jmp core_pointer_type_p
core_macrop:
        mov al, maltype_macro
        jmp core_pointer_type_p
        
core_pointer_type_p:
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        jne .false

        mov rsi, [rsi + Cons.car]
        mov bl, BYTE [rsi]
        cmp bl, al
        jne .false

        ; Check for keyword (not symbol)
        cmp al, maltype_symbol
        jne .true

        mov al, BYTE [rsi + Array.data]
        cmp al, ':'
        je .false               ; a keyword
        
.true:
        ; Return true
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret

.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret

;; Tests if argument is a keyword
core_keywordp:
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        jne .false

        mov rsi, [rsi + Cons.car]
        mov bl, BYTE [rsi]
        cmp bl, maltype_symbol
        jne .false

        ; Check if first character is ':'
        mov bl, BYTE [rsi + Array.data]
        cmp bl, ':'
        jne .false

        ; Return true
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret

.false:
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret

;; Change the value of an atom
core_reset:
        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        jne .not_atom

        ; Get the atom
        mov rax, [rsi + Cons.car] ; Atom in RAX
        mov bl, BYTE [rax]
        cmp bl, maltype_atom
        jne .not_atom

        ; Get the next argument
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_value
        
        mov rsi, [rsi + Cons.cdr]
        
        ; Got something in RSI
        ; release the current value of the atom
        push rax
        push rsi

        mov rsi, [rax + Cons.car] ; The value the atom points to
        call release_object

        pop rsi
        pop rax
        
        ; Check the type of the first argument
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        je .pointer

        ; A value
        
        ; make a copy
        push rax
        push rsi
        push rbx
        call alloc_cons
        pop rbx

        mov bl, bh
        mov [rax], BYTE bl      ; Set type
        
        mov rbx, rax
        pop rsi
        pop rax
        
        mov rcx, [rsi + Cons.car]
        mov [rbx + Cons.car], rcx ; Set value
        
        ; Set the atom to point to it
        mov [rax + Cons.car], rbx
        
        ; Increment refcount since return value will be released
        mov rsi, rbx
        call incref_object
        mov rax, rsi
        ret
        
.pointer:
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx

        mov rsi, rbx
        call incref_object      ; Storing in atom
        call incref_object      ; Returning
        mov rax, rsi
        ret
        
.not_atom:
        ; Not an atom, so throw an error
        mov rsi, core_reset_not_atom
        mov edx, core_reset_not_atom.len
        call raw_to_symbol
        mov rsi, rax
        jmp error_throw

.no_value:
        ; No value given
        mov rsi, core_reset_no_value
        mov edx, core_reset_no_value.len
        call raw_to_symbol
        mov rsi, rax
        jmp error_throw

;; Applies a function to an atom, along with optional arguments
;;
;; In RSI should be a list consisting of
;;  [ atom, pointer->Function , args...]
;;
;; The atom is dereferenced, and inserted into the list:
;; 
;;  [ pointer->Function , atom value , args...]
;;
;; This is then passed to eval.list_exec
;; which executes the function
;;
core_swap:
        ; Check the type of the first argument (an atom)
        mov bl, BYTE [rsi]
        mov bh, bl
        and bh, content_mask
        cmp bh, content_pointer
        jne .not_atom

        ; Get the atom
        mov r9, [rsi + Cons.car] ; Atom in R9
        mov bl, BYTE [r9]
        cmp bl, maltype_atom
        jne .not_atom

        ; Get the second argument (a function)
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_function

        mov rsi, [rsi + Cons.cdr] ; List with function first

        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .no_function

        mov r8, [rsi + Cons.car] ; Function in R8
        mov al, BYTE [r8]
        cmp al, maltype_function
        jne .no_function
        
        ; Get a new Cons 
        ; containing the value in the atom
        call alloc_cons         ; In RAX

        ; Prepend to the list
        mov bl, BYTE [rsi + Cons.typecdr]
        mov [rax + Cons.typecdr], bl
        cmp bl, content_pointer
        jne .done_prepend

        ; A pointer to more args,

        mov rcx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rcx
        
        ; increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.done_prepend:
        
        ; Now get the value in the atom
        mov rdx, [r9 + Cons.car] ; The object pointed to
        
        ; Check what it is
        mov bl, BYTE [rdx]
        mov bh, bl
        and bh, (block_mask + container_mask)
        jz .atom_value         ; Just a value

        ; Not a simple value, so point to it
        mov [rax + Cons.car], rdx
        mov [rax], BYTE (container_list + content_pointer)

        ; Since the list will be released after eval
        ; we need to increment the reference count
        mov bx, WORD [rdx + Cons.refcount]
        inc bx
        mov [rdx + Cons.refcount], WORD bx
        
        jmp .run
        
.atom_value:
        ; Copy the value
        mov rcx, [rdx + Cons.car]
        mov [rax + Cons.car], rcx
        and bl, content_mask    ; keep just the content
        or bl, container_list   ; mark as part of a list
        mov [rax], BYTE bl

.run:
        mov rsi, rax
        
        ; Here have function in R8, args in RSI
        ; Check whether the function is built-in or user
        mov rax, [r8 + Cons.car]
        cmp rax, apply_fn
        je .user_function
        
        ; A built-in function
        push r9                 ; atom
        push rsi                ; Args
        
        call rax
        ; Result in RAX

        pop rsi
        pop r9

        push rax
        call release_object     ; Release arguments
        pop rax
        
        jmp .got_return
        
.user_function:
        ; a user-defined function, so need to evaluate
        ; RSI - Args
        
        mov rdi, r8             ; Function in RDI
        mov rdx, rsi            ; Release args after binding

        mov rsi, r15            ; Environment
        call incref_object      ; Released by eval
        call incref_object      ; also released from R13
        mov r13, r15

        mov rsi, rdx
        
        push r9
        call apply_fn           ; Result in RAX
        pop r9
        
.got_return:
        ; Have a return result in RAX
        
        ; release the current value of the atom
        push rax                ; The result
        mov rsi, [r9 + Cons.car]
        call release_object
        pop rax
        
        ; Put into atom
        mov [r9 + Cons.car], rax
        
        ; Increase reference of new object
        ; because when it is returned it will be released
        mov bx, WORD [rax + Cons.refcount]
        inc bx
        mov [rax + Cons.refcount], WORD bx
        
        ret
        
.not_atom:
        load_static core_swap_not_atom
        jmp core_throw_str
.no_function:
        load_static core_swap_no_function
        jmp core_throw_str


;; Takes two arguments, and prepends the first argument onto the second
;; The second argument can be a list or a vector, but the return is always
;; a list
core_cons:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .missing_args

        mov r8, rsi             ; The object to prepend

        ; Check if there's a second argument
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_args
        
        mov rsi, [rsi + Cons.cdr]

        ; Check that the second argument is a list or vector
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_vector
        
        mov r9, [rsi + Cons.car] ; Should be a list or vector
        mov al, BYTE [r9]
        and al, container_mask
        cmp al, container_list
        je .got_args
        cmp al, container_vector
        je .got_args
        jmp .not_vector
        
.got_args:
        ; Got an object in R8 and list/vector in R9
        
        call alloc_cons         ; new Cons in RAX

        ; Mark as the same content in a list container
        mov bl, BYTE [r8]
        and bl, content_mask
        mov bh, bl              ; Save content in BH for checking if pointer later
        or bl, block_cons + container_list
        mov [rax], BYTE bl
        
        ; Copy the content
        mov rcx, [r8 + Cons.car] ; Content in RCX
        mov [rax + Cons.car], rcx

        ; Check if R9 is empty
        mov dl, BYTE [r9]
        and dl, content_mask
        cmp dl, content_empty
        je .end_append          ; Don't append the list
        
        ; Put the list into CDR
        mov [rax + Cons.cdr], r9
        ; mark CDR as a pointer
        mov [rax + Cons.typecdr], BYTE content_pointer

        ; Increment reference count
        push rax
        mov rsi, r9
        call incref_object
        pop rax
        
.end_append:
        ; Check if the new Cons contains a pointer
        mov bl, BYTE [rax]
        and bl, content_mask
        cmp bl, content_pointer
        jne .done

        ; A pointer, so increment number of references
        push rax
        mov rsi, rcx
        call incref_object
        pop rax
.done:
        ret
        
.missing_args:
        load_static core_cons_missing_arg
        jmp core_throw_str
        
.not_vector:
        load_static core_cons_not_vector
        jmp core_throw_str


;; Concatenate lists, returning a new list
;;
;; Notes:
;;    * The last list does not need to be copied, but all others do
;;
core_concat:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .missing_args

        cmp al, content_pointer
        jne .not_list
        
        ; Check if there is only one argument
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        je .start_loop                ; Start copy loop

        ; Only one input.
        mov rsi, [rsi + Cons.car]
        
        ; Check if it's a list or vector
        mov al, BYTE [rsi]
        mov cl, al
        and al, container_mask
        cmp al, (block_cons + container_list)
        je .single_list

        cmp al, (block_cons + container_vector)
        jne .not_list           ; not a list or vector

        ; A vector. Need to create a new Cons
        ; for the first element, to mark it as a list
        
        call alloc_cons
        and cl, content_mask
        or cl, container_list
        mov [rax], BYTE cl      ; Set type

        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx ; Set content

        ; Check if CAR is a pointer
        cmp cl, (container_list + content_pointer)
        jne .single_done_car

        ; a pointer, so increment reference count
        mov cx, WORD [rbx + Cons.refcount]
        inc cx
        mov [rbx + Cons.refcount], WORD cx
        
.single_done_car:
        mov dl, BYTE [rsi + Cons.typecdr]
        mov [rax + Cons.typecdr], BYTE dl ; CDR type
        
        mov rbx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rbx ; Set CDR content

        ; Check if CDR is a pointer
        cmp dl, content_pointer
        je .single_vector_incref
        ; not a pointer, just return
        ret
        
.single_vector_incref:
        ; increment the reference count of object pointed to
        mov r12, rax            ; The return Cons
        mov rsi, rbx            ; The object address
        call incref_object
        mov rax, r12
        ret
        
.single_list:
        ; Just increment reference count and return
        
        call incref_object
        mov rax, rsi
        ret

.start_loop:  ; Have at least two inputs
        xor r11, r11            ; Head of list. Start in R12
        
.loop:  
        
        ; Check the type
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_list
        
        ; Check if this is the last
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .last

        ; Check if the list is empty
        mov rbx, [rsi + Cons.car] ; The list
        mov al, BYTE [rbx]
        and al, content_mask
        cmp al, content_empty   ; If empty list or vector
        je .next                ; Skip to next
        
        ; not the last list, so need to copy

        push rsi
        mov rsi, rbx ; The list
        call cons_seq_copy        ; Copy in RAX, last Cons in RBX
        pop rsi

        ; Check if this is the first
        test r11, r11
        jnz .append

        ; First list
        mov r11, rbx            ; Last Cons in list
        mov r12, rax            ; Output list
        jmp .next
.append:
        ; End of previous list points to start of new list
        mov [r11 + Cons.cdr], rax 
        mov [r11 + Cons.typecdr], BYTE content_pointer
        ; Put end of new list into R11
        mov r11, rbx
        
.next:
        mov rsi, [rsi + Cons.cdr]
        jmp .loop

.last:
        ; last list, so can just append
        mov rsi, [rsi + Cons.car]

        ; Check if the list is empty
        mov al, BYTE [rsi]
        mov ah, al
        and al, content_mask
        cmp al, content_empty   ; If empty list or vector
        je .done                ; Omit the empty list
        
        call incref_object
        
        mov [r11 + Cons.cdr], rsi
        mov [r11 + Cons.typecdr], BYTE content_pointer
.done:
        ; Check there is anything to return
        test r11, r11
        jz .empty_list
        
        ; Make sure that return is a list
        mov bl, BYTE [r12]
        and bl, content_mask
        or bl, container_list
        mov [r12], BYTE bl
        mov rax, r12            ; output list
        
        ret

.empty_list:
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret
        
.missing_args:
        ; Return empty list
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret
        
.not_list:
        ; Got an argument which is not a list
        mov rsi, core_concat_not_list
        mov edx, core_concat_not_list.len
        
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw

;; Convert a sequence to vector
core_vec:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .error
        mov rsi, [rsi + Cons.car]

        mov al, BYTE [rsi]
        and al, block_mask + container_mask

        ;; delegate lists to `vector` built-in
        cmp al, container_list
        je core_vector

        ;; expect a sequence
        cmp al, container_vector
        jne .error

        ;; return vectors unchanged
        call incref_object
        mov rax, rsi
        ret

.error
        push rsi
        print_str_mac error_string
        print_str_mac core_vec_wrong_arg
        pop rsi
        jmp error_throw

;; Returns the first element of a list
;;
core_first:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .missing_args

        cmp al, content_nil
        je .return_nil
        
        cmp al, content_pointer
        jne .not_list
        
        ; Get the list
        mov rsi, [rsi + Cons.car]

        mov al, BYTE [rsi]

        ; Check for nil
        cmp al, maltype_nil
        je .return_nil
        
        mov ah, al
        and ah, (block_mask + container_mask)
        cmp ah, container_list
        je .got_list
        cmp ah, container_vector
        jne .not_list           ; Not a list or vector
        
.got_list:
        ; Check if list is empty
        and al, content_mask
        cmp al, content_empty
        je .return_nil

        cmp al, content_pointer
        je .return_pointer

        ; Returning a value, so need to copy
        mov cl, al
        call alloc_cons
        mov [rax], BYTE cl      ; Set type

        ; Copy value
        mov rcx, [rsi + Cons.car]
        mov [rax + Cons.car], rcx
        ret
        
.return_pointer:
        mov rsi, [rsi + Cons.car]
        call incref_object
        mov rax, rsi
        ret
        
.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        
.missing_args:
        mov rsi, core_first_missing_arg
        mov edx, core_first_missing_arg.len
        jmp .throw
        
.not_list:
        mov rsi, core_first_not_list
        mov edx, core_first_not_list.len
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw
    

;; Return a list with the first element removed
core_rest:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .missing_args

        cmp al, content_nil
        je .empty_list
        
        cmp al, content_pointer
        jne .not_list

        ; Get the list
        mov rsi, [rsi + Cons.car]
        
        mov al, BYTE [rsi]

        ; Check for nil
        cmp al, maltype_nil
        je .return_nil
        
        mov ah, al
        and ah, (block_mask + container_mask)
        cmp ah, container_list
        je .got_list
        cmp ah, container_vector
        jne .not_list           ; Not a list or vector
        
.got_list:
        ; Check if list or vector is empty
        and al, content_mask
        cmp al, content_empty
        je .empty_list

        ; Check if there is more in the list
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        je .return_rest
        
        ; No more list, so return empty list
.empty_list:
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret
        
.return_rest:
        
        mov rsi, [rsi + Cons.cdr]

        
        
        ; Check if this is a list or a vector
        mov cl, BYTE [rsi]
        mov ch, cl
        and ch, container_mask
        cmp ch, container_list
        je .return_list

        ; Need to allocate a new Cons to replace this first element
        call alloc_cons
        and cl, content_mask
        mov ch, cl              ; Save CAR content type in ch
        or cl, container_list   ; Keep content type, set container type to list
        mov [rax], BYTE cl

        mov dl, BYTE [rsi + Cons.typecdr] ; CDR type in DL
        mov [rax + Cons.typecdr], BYTE dl
        
        ; Copy content of CAR
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx
        
        ; Check if car contains a pointer
        cmp ch, content_pointer
        jne .check_cdr
        
        ; CAR contains a pointer, so increment reference count
        
        mov r8, rax             ; Save return Cons
        mov r9, rsi             ; Save input list
        mov rsi, rbx            ; Content of CAR
        call incref_object
        mov rax, r8             ; Restore return Cons
        mov rsi, r9             ; Restore input list
        
.check_cdr:
        ; Copy content of CDR
        
        mov rcx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rcx ; Note: Might be pointer
        
        ; Check if cdr contains a pointer
        cmp dl, content_pointer
        jne .return             ; Not a pointer, so just return
        
        ; A pointer, so increment its reference count
        mov rbx, rax            ; Save the return Cons
        mov rsi, rcx            ; The pointer in CDR
        call incref_object
        mov rax, rbx            ; Restore the return Cons
        ret
        
.return_list:
        call incref_object
        mov rax, rsi
.return:
        ret

.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        
.missing_args:
        mov rsi, core_rest_missing_arg
        mov edx, core_rest_missing_arg.len
        jmp .throw
        
.not_list:
        mov rsi, core_rest_not_list
        mov edx, core_rest_not_list.len
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw


;; Return the nth element of a list or vector
core_nth:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .missing_args

        cmp al, content_nil
        je .return_nil
        
        cmp al, content_pointer
        jne .not_list

        ; Get the list into R8
        mov r8, [rsi + Cons.car]
        
        ; Check if we have a second argument
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_args

        mov r9, [rsi + Cons.cdr]
        
        ; Check that it is a number
        mov al, BYTE [r9]
        and al, content_mask
        cmp al, content_int
        jne .not_int

        ; Get the number in RBX
        mov rbx, [r9 + Cons.car]

        ; Now loop through the list, moving along n elements
.loop:
        test rbx, rbx           ; Test if zero
        jz .done

        ; Move along next element

        mov al, BYTE [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .out_of_range       ; No element

        mov r8, [r8 + Cons.cdr]
        dec rbx
        jmp .loop        
        
.done:
        ; Take the head of the list in R8
        mov al, BYTE [r8]
        and al, content_mask
        cmp al, content_pointer
        je .return_pointer

        ; Copy a value
        mov cl, al
        call alloc_cons
        mov [rax], BYTE cl
        mov rcx, [r8 + Cons.car]
        mov [rax + Cons.car], rcx
        ret
        
.return_pointer:
        mov rsi, [r8 + Cons.car]
        call incref_object
        mov rax, rsi
        ret

.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        
.missing_args:
        mov rsi, core_nth_missing_arg
        mov edx, core_nth_missing_arg.len
        jmp .throw
        
.not_list:
        mov rsi, core_nth_not_list
        mov edx, core_nth_not_list.len
        jmp .throw

.not_int:
        mov rsi, core_nth_not_int
        mov edx, core_nth_not_int.len
        jmp .throw
        
.out_of_range:
        mov rsi, core_nth_out_of_range
        mov edx, core_nth_out_of_range.len
        
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw

;; Check if the argument is a given value type
core_nilp:
        mov al, BYTE content_nil
        jmp core_value_type_p
core_truep:
        mov al, BYTE content_true
        jmp core_value_type_p
core_falsep:
        mov al, BYTE content_false
        jmp core_value_type_p
core_numberp:
        mov al, BYTE content_int
;; predicates for nil, true, false and number jump here
core_value_type_p:
        mov bl, BYTE [rsi]
        and bl, content_mask
        cmp bl, content_empty
        je .missing_args

        cmp al, bl
        je .true
        
        ; false
        call alloc_cons
        mov [rax], BYTE maltype_false
        ret
.true:
        call alloc_cons
        mov [rax], BYTE maltype_true
        ret
        
.missing_args:
        mov rsi, core_value_p_missing_args
        mov edx, core_value_p_missing_args.len
        
        call raw_to_string
        mov rsi, rax
        jmp error_throw

;; Throws an exception
core_throw:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_empty
        je .throw_nil          ; No arguments

        cmp al, content_pointer
        je .throw_pointer

        ; A value. Remove list content type
        mov [rsi], BYTE al
        jmp error_throw
        
.throw_pointer:
        mov rsi, [rsi + Cons.car]
        jmp error_throw
        
.throw_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov rsi, rax
        jmp error_throw
        
;; Applies a function to a list or vector
;;
;; Uses registers
;;    R8  - function
;;    R9  - Input list/vector
;;    R10  - Current end of return list (for appending)
core_map:
        xor r10,r10             ; Zero, signal no list
        
        ; First argument should be a function
        mov bl, BYTE [rsi]
        and bl, content_mask
        cmp bl, content_empty
        je .missing_args

        ; Check the first argument is a pointer
        cmp bl, content_pointer
        jne .not_function
        
        mov r8, [rsi + Cons.car] ; Function in R8
        mov bl, BYTE [r8]
        cmp bl, maltype_function
        jne .not_function

        ; Check for second argument
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .missing_args

        mov rsi, [rsi + Cons.cdr]
        
        ; Should be a pointer to a list or vector
        mov bl, BYTE [rsi]
        and bl, content_mask
        cmp bl, content_pointer
        jne .not_seq

        mov r9, [rsi + Cons.car] ; List or vector in R9

        mov bl, BYTE [r9]

        mov bh, bl
        and bh, content_mask
        cmp bh, content_empty
        je .empty_list
        
        and bl, (block_mask + container_mask)
        cmp bl, container_list
        je .start
        cmp bl, container_vector
        je .start
        
        ; not list or vector
        jmp .not_seq
        
.start:
        ; Got function in R8, list or vector in R9
        
        mov cl, BYTE [r9]
        and cl, content_mask
        mov ch, cl
        or cl, container_list
        
        call alloc_cons
        mov [rax], BYTE cl      ; set content type
        mov rbx, [r9 + Cons.car]
        mov [rax + Cons.car], rbx ; Copy content
        mov rsi, rax

        cmp ch, content_pointer
        jne .run

        ; A pointer, so increment ref count

        mov rcx, rsi
        mov rsi, rbx
        call incref_object
        mov rsi, rcx
        
.run:
        ; Here have function in R8, args in RSI
        ; Check whether the function is built-in or user
        mov rax, [r8 + Cons.car]
        cmp rax, apply_fn
        je .user_function
        
        ; A built-in function
        push r8                 ; function
        push r9                 ; input list/vector
        push r10                ; End of return list
        push rsi
        
        call rax
        ; Result in RAX

        pop rsi
        pop r10
        pop r9
        pop r8

        push rax
        call release_object     ; Release arguments
        pop rax
        
        jmp .got_return
        
.user_function:
        ; a user-defined function, so need to evaluate
        ; RSI - Args
        
        mov rdi, r8             ; Function in RDI
        mov rdx, rsi            ; Release args after binding

        mov rsi, r15            ; Environment
        call incref_object      ; Released by eval
        call incref_object      ; also released from R13
        mov r13, r15

        mov rsi, rdx
        
        push r8
        push r9
        push r10
        push r15
        call apply_fn           ; Result in RAX
        pop r15
        pop r10
        pop r9
        pop r8
        
.got_return:
        ; Have a return result in RAX
        
        ; Check if it's a value type
        mov bl, BYTE [rax]
        mov bh, bl
        and bl, (block_mask + container_mask)
        jz .return_value

        ; A more complicated type, point to it
        mov rcx, rax
        call alloc_cons         ; Create a Cons for address
        mov [rax], BYTE (container_list + content_pointer)
        mov [rax + Cons.car], rcx
        jmp .update_return
        
.return_value:
        ; Check if this value is shared (e.g. in an atom)
        mov cx, WORD [rax + Cons.refcount]
        dec cx
        jz .return_value_modify ; If reference count is 1
        
        ; Need to copy to avoid modifying
        push rsi
        mov rsi, rax            ; Original in RSI

        mov cl, bh              ; Type
        call alloc_cons
        and cl, content_mask
        or cl, container_list
        mov [rax], BYTE cl      ; mark as a list
        
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx ; copy content
        
        ; Release original
        push rax
        call release_object
        pop rax
        pop rsi
        
        jmp .update_return
        
.return_value_modify:
        ; Only one reference,
        ; so can change the container type to list.
        ; Original type in bh
        mov bl, bh
        and bl, content_mask
        or bl, container_list
        mov [rax], BYTE bl
        
.update_return:
        ; Now append to result list
        test r10,r10
        jnz .append

        ; First value
        mov r10, rax            ; End of list
        push r10                ; popped before return
        jmp .next
.append:
        mov [r10 + Cons.cdr], rax ; Point to new Cons
        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov r10, rax
.next:
        ; Check if there is another value
        mov al, [r9 + Cons.typecdr]
        cmp al, content_pointer
        jne .done               ; no more 

        mov r9, [r9 + Cons.cdr] ; next
        jmp .start
        
.done:
        pop rax                 ; Pushed in .update_return
        ret
        
.empty_list:
        ; Got an empty list, so return an empty list
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret
        
.missing_args:
        ; Either zero or one args, expect two
        load_static core_map_missing_args
        jmp core_throw_str
.not_function:
        ; First argument not a function
        load_static core_map_not_function
        jmp core_throw_str
.not_seq:
        ; Second argument not list or vector
        load_static core_map_not_seq
        jmp core_throw_str
        

;; Applies a function to a list of arguments, concatenated with
;; a final list of args
;; (function, ..., [])
core_apply:
        ; First argument should be a function
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_function

        mov r8, [rsi + Cons.car] ; function in R8
        mov al, BYTE [r8]
        cmp al, maltype_function
        jne .not_function

        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_args
        
        xor r9,r9
        ; Optional args, followed by final list/vector
.loop:
        mov rsi, [rsi + Cons.cdr]
        
        ; Check if this is the last
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .last

        ; Not the last, so copy
        call alloc_cons         ; New Cons in RAX
        mov bl, BYTE [rsi]
        mov [rax], BYTE bl
        mov rcx, [rsi + Cons.car]
        mov [rax + Cons.car], rcx

        and bl, content_mask
        cmp bl, content_pointer
        jne .got_value

        ; A pointer, so increment reference
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.got_value:
        ; Now append this Cons to the list
        test r9,r9
        jnz .append

        ; First
        mov r9, rax             ; Start of the list
        mov r10, rax            ; End of the list
        jmp .loop
        
.append:
        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov [r10 + Cons.cdr], rax
        mov r10, rax
        jmp .loop
        
.last:
        ; Check that it's a list or vector
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_seq
        
        mov rsi, [rsi + Cons.car] ; Vector/list in RSI
        mov al, BYTE [rsi]
        and al, container_mask
        cmp al, container_list
        je .last_seq
        cmp al, container_vector
        jne .not_seq
        
.last_seq:
        ; Check if there were any previous args
        test r9, r9
        jnz .last_append

        ; R9 is zero, so no previous args

        ; check that this is a list
        ; and convert vector to list

        mov r9, rsi
        
        ; Check if R9 is a list
        mov al, BYTE [r9]
        mov cl, al
        and al, container_mask
        cmp al, container_list
        jne .last_convert_to_list

        ; Already a list, just increment reference count
        mov rsi, r9
        call incref_object
        jmp .run

.last_convert_to_list:
        ; Convert vector to list by copying first element

        call alloc_cons
        and cl, content_mask
        or cl, container_list
        mov [rax], BYTE cl
        mov rdx, [r9 + Cons.car]
        mov [rax + Cons.car], rdx
        
        ; check if contains a pointer
        cmp cl, (container_list + content_pointer)
        jne .copy_cdr
        
        ; A pointer, so increment reference
        mov bx, WORD [rdx + Cons.refcount]
        inc bx
        mov [rdx + Cons.refcount], WORD bx
        
.copy_cdr:
        mov bl, BYTE [r9 + Cons.typecdr]
        mov rcx, [r9 + Cons.cdr]
        mov [rax + Cons.typecdr], BYTE bl
        mov [rax + Cons.cdr], rcx

        ; Replace R9 with this new element
        mov r9, rax
        
        cmp bl, content_pointer
        jne .run

        ; A pointer, so increment reference
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
        jmp .run
        
.last_append:
        ; Append RSI to the end of the list [R9]...[R10]
        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov [r10 + Cons.cdr], rsi
        call incref_object
        
.run:
        ; Have arguments list in R9
        mov rsi, r9
        ; Here have function in R8, args in RSI
        ; Check whether the function is built-in or user
        mov rax, [r8 + Cons.car]
        cmp rax, apply_fn
        je .user_function
        
        ; A built-in function
        push r8                 ; function
        push r9                 ; input list/vector
        push r10                ; End of return list
        push rsi
        
        call rax
        ; Result in RAX

        pop rsi
        pop r10
        pop r9
        pop r8

        push rax
        call release_object     ; Release arguments
        pop rax
        
        ret
        
.user_function:
        ; a user-defined function, so need to evaluate
        ; RSI - Args
        
        mov rdi, r8             ; Function in RDI
        mov rdx, rsi            ; Release args after binding

        mov rsi, r15            ; Environment
        call incref_object      ; Released by eval
        call incref_object      ; also released from R13
        mov r13, r15

        mov rsi, rdx
        
        push r8
        push r9
        push r10
        call apply_fn           ; Result in RAX
        pop r10
        pop r9
        pop r8
        
        ret
                
.not_function:
        load_static core_apply_not_function
        jmp core_throw_str

.missing_args:
        load_static core_apply_missing_args
        jmp core_throw_str

.not_seq:
        load_static core_apply_not_seq
        jmp core_throw_str

;; Converts a string to a symbol
core_symbol:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_string

        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        cmp al, maltype_string
        jne .not_string

        ; Copy the string
        call string_copy        ; result in RAX

        mov [rax], BYTE maltype_symbol
        ret
        
.not_string:
        load_static core_symbol_not_string
        jmp core_throw_str

;; Converts a string to a keyword
core_keyword:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_string

        mov r8, [rsi + Cons.car] ; String in R8
        mov al, BYTE [r8]
        cmp al, maltype_string
        jne .not_string

        call string_new         ; String in RAX
        mov rsi, rax
        mov cl, ':'
        call string_append_char ; Puts ':' first
        
        mov rdx, r8
        call string_append_string ; append

        ; Mark as keyword
        mov [rsi], BYTE maltype_symbol
        
        mov rax, rsi
        ret
        
.not_string:
        load_static core_keyword_not_string
        jmp core_throw_str

;; Sets values in a map
core_assoc:
        ; check first arg
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_map
        
        mov r8, [rsi + Cons.car] ; map in R8
        mov al, BYTE [r8]
        and al, container_mask
        cmp al, container_map
        jne .not_map

        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        je .start

        ; No keys to set, so just increment and return
        mov rsi, r8
        call incref_object
        mov rax, rsi
        ret
        
.start:
        mov r11, [rsi + Cons.cdr] ; List of keys/values in R11

        ; Copy the original list
        mov rsi, r8
        call map_copy
        mov rsi, rax            ; new map in RSI

.loop:
        ; Get key then value from R11 list

        mov cl, BYTE [r11]
        and cl, content_mask
        cmp cl, content_pointer
        je .key_pointer

        ; Key is a value, so copy into a Cons
        call alloc_cons
        mov [rax], BYTE cl
        mov rbx, [r11 + Cons.car]
        mov [rax + Cons.car], rbx
        mov rdi, rax            ; Key in RDI
        jmp .get_value
        
.key_pointer:
        mov rdi, [r11 + Cons.car]
        ; increment reference count because the key will be
        ; released after setting (to allow value Cons to be
        ; freed)

        mov bx, WORD [rdi + Cons.refcount]
        inc bx
        mov [rdi + Cons.refcount], WORD bx
        
.get_value:
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_value

        mov r11, [r11 + Cons.cdr]

        ; Check if value is a pointer
        mov cl, BYTE [r11]
        and cl, content_mask
        cmp cl, content_pointer
        je .value_pointer
        
        ; Value is a value, so copy into a Cons
        call alloc_cons
        mov [rax], BYTE cl
        mov rbx, [r11 + Cons.car]
        mov [rax + Cons.car], rbx
        mov rcx, rax            ; Key in RCX
        jmp .set_pair

.value_pointer:
        mov rcx, [r11 + Cons.car]
        ; increment reference count because the value will be
        ; released after setting (to allow value Cons to be
        ; freed)

        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.set_pair:
        ; Here have:
        ; map in RSI
        ; key in RDI
        ; value in RCX
        
        call map_set

        mov r8, rsi             ; map
        mov rsi, rdi            ; key
        call release_object
        mov rsi, rcx            ; value
        call release_object
        mov rsi, r8             ; map

        ; Check if there's another pair
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .done
        
        ; got another pair
        mov r11, [r11 + Cons.cdr]
        jmp .loop
        
.done:
        mov rax, rsi            ; new map
        ret
        
.not_map:
        load_static core_assoc_not_map
        jmp core_throw_str
        
.missing_value:
        load_static core_assoc_missing_value
        jmp core_throw_str


;; Removes keys from a map by making
;; a copy of a map without the given keys
core_dissoc:
        ; Check that the first argument is a map
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .not_map

        mov r8, [rsi + Cons.car] ; Map in R8
        mov al, BYTE [r8]
        mov ah, al
        and al, container_mask
        cmp al, container_map
        jne .not_map

        ; Check if the map is empty
        cmp ah, maltype_empty_map
        je .inc_and_return
        
        ; Now check if there are other arguments

        mov al, [rsi + Cons.typecdr]
        cmp al, content_pointer
        je .start

.inc_and_return:
        ; No keys to remove
        ; just increment the map reference count and return
        mov rsi, r8
        call incref_object
        mov rax, rsi
        ret
        
.start:
        ; Some keys to remove
        mov r9, [rsi + Cons.cdr]

        ; R9 now contains a list of keys
        ; R8 contains the map to copy

        xor r11, r11            ; Head of list to return
        ; R12 contains tail
        
.loop:  
        ; Check the key in R8 against the list in R9
        mov r10, r9             ; point in list being searched

        ; loop through the list in R10
        ; comparing each element against R8
.search_loop:
        mov rsi, r8
        mov rdi, r10
        call compare_objects
        test rax, rax
        jz .found               ; objects are equal
        
        ; Not found so check next in list
        mov al, BYTE [r10 + Cons.typecdr]
        cmp al, content_pointer
        jne .not_found          ; End of list
        
        mov r10, [r10 + Cons.cdr] ; next
        jmp .search_loop
        
.found:
        ; Removing this key, so skip
        mov al, BYTE [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_value
        
        mov r8, [r8 + Cons.cdr] ; now a value
        jmp .next
        
.not_found:
        ; Key not in list, so keeping
        ; Create a Cons to copy
        call alloc_cons
        mov bl, [r8]
        mov rcx, [r8 + Cons.car]

        mov [rax], BYTE bl
        mov [rax + Cons.car], rcx
        
        ; Check if a pointer or value
        and bl, content_mask
        cmp bl, content_pointer
        jne .done_key           ; A value

        ; a pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.done_key:
        ; append to list

        test r11, r11
        jnz .key_append

        ; First one
        mov r11, rax
        mov r12, rax
        jmp .copy_value
.key_append:
        
        mov [r12 + Cons.typecdr], BYTE content_pointer
        mov [r12 + Cons.cdr], rax
        mov r12, rax

.copy_value:

        ; Check there is a value
        mov al, BYTE [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .missing_value

        mov r8, [r8 + Cons.cdr] ; Value

        ; Same as for key; create a Cons and copy
        call alloc_cons
        mov bl, [r8]
        mov rcx, [r8 + Cons.car]

        mov [rax], BYTE bl
        mov [rax + Cons.car], rcx
        
        ; Check if a pointer or value
        and bl, content_mask
        cmp bl, content_pointer
        jne .done_value           ; A value

        ; a pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx

.done_value:
        ; append to list
        mov [r12 + Cons.typecdr], BYTE content_pointer
        mov [r12 + Cons.cdr], rax
        mov r12, rax
        
.next:
        ; Here R8 contains a value
        
        ; Check if there's another key
        mov al, [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .done

        ; Still more

        mov r8, [r8 + Cons.cdr]
        jmp .loop
        
.done:
        ; Check if the map is empty
        test r11, r11
        jz .return_empty
        
        ; not empty, so return
        mov rax, r11
        ret

.return_empty:
        call alloc_cons
        mov [rax], BYTE maltype_empty_map
        ret
        
.not_map:
        load_static core_dissoc_not_map
        jmp core_throw_str

.missing_value:
        load_static core_dissoc_missing_value
        jmp core_throw_str


;; Takes a string prompt for the user, and returns
;; a string or nil
core_readline:
        ; Check the input 
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .no_prompt

        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        cmp al, maltype_string
        jne .no_prompt

        ; Got a string in RSI
        call print_string

.no_prompt:

        ; Get string from user
        call read_line
        
        ; Check if we have a zero-length string
        cmp DWORD [rax+Array.length], 0
        je .return_nil

        ; return the string in RAX
        ret
        
.return_nil:
        ; release string in RAX
        mov rsi, rax
        call release_array

        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret


;; Return the meta data associated with a given function
core_meta:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .return_nil

        mov rsi, [rsi + Cons.car]
        mov al, BYTE [rsi]
        cmp al, (block_cons + container_function + content_function)
        jne .return_nil

        ; Here got a function
        mov rsi, [rsi + Cons.cdr]

        ; RSI should now contain the meta data
        mov cl, BYTE [rsi]
        and cl, content_mask
        cmp cl, content_pointer
        je .pointer

        ; A value, so copy
        call alloc_cons
        mov [rax], BYTE cl
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx
        ret
        
.pointer:
        ; A pointer, so increment reference count and return
        mov rsi, [rsi + Cons.car]
        call incref_object
        mov rax, rsi
        ret
        
.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        

;; Associates a value with a function (native or user)
core_with_meta:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .no_function

        mov r8, [rsi + Cons.car] ; Function in R8
        mov al, BYTE [r8]
        cmp al, (block_cons + container_function + content_function)
        jne .no_function
        
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_value
        
        mov rsi, [rsi + Cons.cdr]

        ; Function in R8, new value in RSI

        call alloc_cons
        mov [rax], BYTE (block_cons + container_function + content_function)      ; Type
        mov rbx, [r8 + Cons.car]
        mov [rax + Cons.car], rbx ; Function address
        
        mov r10, rax            ; Return address

        ; Copy the meta data

        mov r8, [r8 + Cons.cdr] ; R8 now old meta data (not used)
        
        call alloc_cons
        
        mov cl, BYTE [rsi]
        and cl, content_mask
        mov ch, cl
        or cl, container_function
        mov [rax], BYTE cl      ; Set type

        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx ; Copy value

        ; append to function
        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov [r10 + Cons.cdr], rax
        mov r11, rax
        
        ; Check if meta is a value or pointer
        cmp ch, content_pointer
        jne .copy_rest

        ; increment reference count of meta
        mov cx, WORD [rbx + Cons.refcount]
        inc cx
        mov [rbx + Cons.refcount], WORD cx
        
.copy_rest:
        ; Copy remainder of function (if any)
        ; If a user function, has (env binds body)
        mov al, [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .done

        ; Still more to copy
        mov r8, [r8 + Cons.cdr]
        
        call alloc_cons
        mov bl, BYTE [r8]
        mov [rax], BYTE bl      ; Copy type
        mov rcx, [r8 + Cons.car]
        mov [rax + Cons.car], rcx ; Copy value

        ; append
        mov [r11 + Cons.typecdr], BYTE content_pointer
        mov [r11 + Cons.cdr], rax
        mov r11, rax

        ; Check if it's a pointer
        and bl, content_mask
        cmp bl, content_pointer
        jne .copy_rest

        ; a pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        jmp .copy_rest
        
.done:
        mov rax, r10
        ret
        
.no_function:
        load_static core_with_meta_no_function
        jmp core_throw_str

.no_value:
        load_static core_with_meta_no_value
        jmp core_throw_str


;; Returns the current time in ms
core_time_ms:
        call clock_time_ms
        mov rsi, rax
        
        call alloc_cons
        mov [rax], BYTE maltype_integer
        mov [rax + Cons.car], rsi
        ret

;; Convert sequences, including strings, into lists
core_seq:
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        je .pointer
        
        cmp al, content_empty
        je .missing_arg

        cmp al, content_nil
        jne .wrong_type
        
.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        
.pointer:
        mov r8, [rsi + Cons.car]
        mov al, BYTE [r8]

        cmp al, maltype_string
        je .string

        mov ah, al
        and ah, (block_mask + content_mask)
        cmp ah, (block_cons + content_empty)
        je .return_nil
        
        and al, (block_mask + container_mask)
        
        cmp al, (block_cons + container_list)
        je .list
        
        cmp al, (block_cons + container_vector)
        jne .wrong_type

        ; Convert vector to list by replacing the first Cons
        call alloc_cons
        mov bl, BYTE [r8]
        and bl, content_mask
        or bl, container_list
        mov [rax], BYTE bl      ; Set type

        mov rcx, [r8 + Cons.car]
        mov [rax + Cons.car], rcx

        ; Check if it's a pointer
        cmp bl, (container_list + content_pointer)
        jne .copy_cdr

        ; Increment reference count
        mov bx, WORD [rcx + Cons.refcount] ; Same for Array
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.copy_cdr:
        mov rcx, [r8 + Cons.cdr]
        mov [rax + Cons.cdr], rcx

        mov bl, [r8 + Cons.typecdr]
        mov [rax + Cons.typecdr], bl

        cmp bl, content_pointer
        jne .return
        
        ; Increment reference count
        mov bx, WORD [rcx + Cons.refcount] ; Same for Array
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.return:
        ret
        
.list:
        ; Return list unchanged
        mov rsi, r8
        call incref_object
        mov rax, r8
        ret

.string:
        ; Split a string into characters
        ; Input string in R8
        
        mov ebx, DWORD [r8 + Array.length]
        test ebx,ebx
        jz .return_nil          ; empty string

        ; Not empty, so allocate first Cons
        call alloc_cons
        mov r9, rax             ; Return Cons in R9
        mov r10, rax            ; End of list in R10

.loop:
        mov ebx, DWORD [r8 + Array.length]
        mov r11, r8
        add r11, Array.data     ; Start of string data in R11
        mov r12, r11
        add r12, rbx            ; End of string data in R12
        
.inner_loop:
        ; Get a new string
        call string_new         ; in RAX
        mov bl, BYTE [r11]      ; Get the next character
        mov [rax + Array.data], BYTE bl
        mov [rax + Array.length], DWORD 1
        
        ; Put string into Cons at end of list
        mov [r10 + Cons.car], rax
        
        ; Set type
        mov [r10], BYTE (container_list + content_pointer)
        
        inc r11
        cmp r11, r12
        je .inner_done
        
        ; more characters, so allocate another Cons
        call alloc_cons

        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov [r10 + Cons.cdr], rax
        mov r10, rax
        jmp .inner_loop
        
.inner_done:
        ; No more characters in this Array
        ; check if there are more
        mov r8, QWORD [r8 + Array.next]     ; Get the next Array address
        test r8, r8           ; Test if it's null
        jz .string_finished

        ; Another chunk in the string
        
        call alloc_cons
        mov [r10 + Cons.typecdr], BYTE content_pointer
        mov [r10 + Cons.cdr], rax
        mov r10, rax
        jmp .loop
        
.string_finished:
        mov rax, r9
        ret
        
.missing_arg:
        ; No arguments
        load_static core_seq_missing_arg
        jmp core_throw_str
        
.wrong_type:
        ; Not a list, vector, string or nil
        load_static core_seq_wrong_type
        jmp core_throw_str
        
