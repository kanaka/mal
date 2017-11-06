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
        
;; Strings

        static core_emptyp_error_string, db "empty? expects a list, vector or map",10
        static core_count_error_string, db "count expects a list or vector",10
        static core_numeric_expect_ints, db "comparison operator expected two numbers",10
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
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .false              ; Should be a pointer to a list

        mov rax, [rsi + Cons.car]
        mov al, BYTE [rax]
        and al, (block_mask + container_mask)
        cmp al, (block_cons + container_list)
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

        ; first string
        mov r8, rax             ; Output string 
        jmp .next
        
.append:
        push rsi
        push rax
        
        mov rsi, r8             ; Output string 
        mov rdx, rax            ; String to be copied
        call string_append_string
        mov r8, rax
        
        pop rsi                 ; Was in rax, temporary string
        call release_array      ; Release the string

        pop rsi                 ; Restore input

.next:
        ; Check if there's another
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done

        ; More inputs
        mov rsi, [rsi + Cons.cdr] ; pointer

        ; Add separator
        push rsi
        mov rsi, r8
        mov cl, ' '
        call string_append_char
        pop rsi

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
        ; Convert to string
        call core_pr_str
        ; print the string
        mov rsi, rax
        push rsi                ; Save the string address
        call print_string
        pop rsi
        call release_array      ; Release the string

        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
