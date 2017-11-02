;; Core functions
;;
;;

%include "macros.mac"
        
section .data

        static core_add_symbol, db "+"
        static core_sub_symbol, db "-"
        static core_mul_symbol, db "*"
        static core_div_symbol, db "/"
        static core_equal_symbol, db "="
        static core_keys_symbol, db "keys"   
        
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

        core_env_native core_equal_symbol, core_equal_p

        core_env_native core_keys_symbol, core_keys
        
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

;; Test objects for equality
core_equal_p:
        ; Check that rsi contains a list
        mov cl, BYTE [rsi]
        and cl, block_mask + container_mask
        cmp cl, block_cons + container_list
        jne .error
        
        ; Check that the list has a second pointer
        mov cl, BYTE [rsi + Cons.typecdr]
        cmp cl, content_pointer
        jne .error
        
        ; move second pointer into rdi
        mov rdi, [rsi + Cons.cdr]

        ; Compare rsi and rdi objects
        call compare_objects    ; result in rax
        
        ; for now put result into Cons
        mov rdi, rax
        call alloc_cons
        mov [rax], BYTE maltype_integer
        mov [rax + Cons.typecdr], BYTE content_nil
        mov [rax + Cons.car], rdi
        ret
.error:
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

;; Given a map, returns a list of keys
;; Input: List in RSI with one Map element
;; Returns: List in RAX
core_keys:
        mov rsi, [rsi + Cons.car]
        call map_keys
        ret
