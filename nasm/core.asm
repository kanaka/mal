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

        static core_containsp_symbol, db "contains?"
        static core_vectorp_symbol, db "vector?"
        static core_mapp_symbol, db "map?"
;; Strings

        static core_emptyp_error_string, db "empty? expects a list, vector or map",10
        static core_count_error_string, db "count expects a list or vector",10
        static core_numeric_expect_ints, db "comparison operator expected two numbers",10

        static core_deref_not_atom, db "Error: argument to deref is not an atom"
        static core_reset_not_atom, db "Error: argument to reset is not an atom"
        static core_reset_no_value, db "Error: missing value argument to reset"

        static core_cons_missing_arg, db "Error: missing argument to cons"
        static core_cons_not_vector, db "Error: cons expects a list or vector"
        
        static core_concat_not_list, db "Error: concat expects lists or vectors"

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

        core_env_native core_cons_symbol, core_cons
        core_env_native core_concat_symbol, core_concat
        
        core_env_native core_first_symbol, core_first
        core_env_native core_rest_symbol, core_rest
        core_env_native core_nth_symbol, core_nth
        
        core_env_native core_nilp_symbol, core_nilp
        core_env_native core_truep_symbol, core_truep
        core_env_native core_falsep_symbol, core_falsep
        core_env_native core_numberp_symbol, core_numberp

        core_env_native core_symbolp_symbol, core_symbolp
        core_env_native core_stringp_symbol, core_stringp
        core_env_native core_fnp_symbol, core_fnp
        core_env_native core_macrop_symbol, core_macrop

        core_env_native core_containsp_symbol, core_containsp

        core_env_native core_vectorp_symbol, core_vectorp
        core_env_native core_mapp_symbol, core_mapp
        
        ; -----------------
        ; Put the environment in RAX
        mov rax, rsi
        ret

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
        jne .error
        mov ch, cl
        and ch, content_mask
        cmp ch, content_int
        jne .error

        ; Put the starting value in rax
        mov rax, [rsi + Cons.car]
        
.add_loop:
        ; Fetch the next value
        mov cl, [rsi + Cons.typecdr]
        cmp cl, content_nil
        je .finished            ; Nothing let
        cmp cl, content_pointer
        jne .error

        mov rsi, [rsi + Cons.cdr] ; Get next cons

        ; Check that it is an integer
        mov cl, BYTE [rsi]
        and cl, content_mask
        cmp cl, content_int
        jne .error

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
.error:
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

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
        mov rsi, [rsi + Cons.car]
        call map_keys
        ret

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
        mov rsi, core_containsp_not_map
        mov edx, core_containsp_not_map.len
        jmp .throw
.no_key:
        mov rsi, core_containsp_no_key
        mov edx, core_containsp_no_key.len
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw
        
;; Return arguments as a list
;; 
core_list:
        call incref_object
        mov rax, rsi
        ret
        
;; ------------------------------------------------
;; String functions

;; Convert arguments to a readable string, separated by a space
;; 
core_pr_str:
        mov rdi, 1              ; print_readably
        jmp core_str_functions
core_str:
        xor rdi, rdi
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

        cmp rdi, 0              ; print_readably
        je .end_append_char     ; No separator if not printing readably
        
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
        call core_str
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

        ; Got an atom, return true
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
        mov r8, [rsi + Cons.car] ; Atom in R8
        mov bl, BYTE [r8]
        cmp bl, maltype_atom
        jne .not_atom

        ; Get the second argument (a function)
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .no_function

        mov r9, [rsi + Cons.cdr] ; List with function first
        
        ; Get a new Cons to insert into the list
        ; containing the value in the atom
        call alloc_cons         ; In RAX

        ; Splice into the list
        mov bl, BYTE [r9 + Cons.typecdr]
        mov rcx, [r9 + Cons.cdr]
        mov [rax + Cons.typecdr], bl
        mov [rax + Cons.cdr], rcx
        mov [r9 + Cons.typecdr], BYTE content_pointer
        mov [r9 + Cons.cdr], rax
        
        ; Now get the value in the atom
        mov rdx, [r8 + Cons.car] ; The object pointed to

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
        mov rsi, rdx
        call incref_object
        
        jmp .list_done
        
.atom_value:
        ; Copy the value
        mov rcx, [rdx + Cons.car]
        mov [rax + Cons.car], rcx
        and bl, content_mask    ; keep just the content
        or bl, container_list   ; mark as part of a list
        mov [rax], BYTE bl

.list_done:
        ; Now have a list with function followed by args
        ; This is the same state as after a call to eval_ast
        ; 
        ; Note: Because eval releases the environment in R15
        ;       on return, this needs to have its references
        ;       incremented before the call
        ;
        ; The list passed in RAX will be released by eval

        mov rsi, r15
        call incref_object

        mov rax, r9
        push r8                 ; The atom
        call eval.list_exec     ; Result in RAX
        pop r8
        
        ; release the current value of the atom
        push rax                ; The result
        mov rsi, [r8 + Cons.car]
        call release_object
        pop rsi

        ; Put into atom
        mov [r8 + Cons.car], rsi
        
        ; Increase reference of new object
        ; because when it is returned it will be released
        push rsi
        call incref_object 
        pop rax
        ret
        
.not_atom:
.no_function:
        xor rsi,rsi
        jmp error_throw


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
        mov rsi, core_cons_missing_arg
        mov edx,core_cons_missing_arg.len
        jmp .throw
        
.not_vector:
        mov rsi, core_cons_not_vector
        mov edx, core_cons_not_vector.len
        
.throw:
        call raw_to_string
        mov rsi, rax
        jmp error_throw


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
        ; Make sure that return is a list
        mov bl, BYTE [r12]
        and bl, content_mask
        or bl, container_list
        mov [r12], BYTE bl
        mov rax, r12            ; output list
        
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
        ; Check if list is empty
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
        
        ; Copy content of CAR and CDR
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx

        mov rcx, [rsi + Cons.cdr]
        mov [rax + Cons.cdr], rcx ; Note: Might be pointer

        ; Check if car contains a pointer
        cmp ch, content_pointer
        jne .check_cdr

        ; CAR contains a pointer, so increment reference count
        
        mov r8, rax             ; Save return Cons
        mov rsi, rbx            ; Content of CAR
        call incref_object
        mov rax, r8             ; Restore return Cons
        
.check_cdr:        
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

        
