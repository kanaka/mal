;; 
;; nasm -felf64 step2_eval.asm && ld step2_eval.o && ./a.out

;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
global  _start
        
%include "types.asm"            ; Data types, memory
%include "system.asm"           ; System calls
%include "reader.asm"           ; String -> Data structures
%include "printer.asm"          ; Data structures -> String
%include "exceptions.asm"       ; Error handling
        
section .bss
        
;; Top-level (REPL) environment
repl_env:resq 1

section .data

;; ------------------------------------------
;; Fixed strings for printing
        
        static prompt_string, db 10,"user> "      ; The string to print at the prompt

        static error_string, db 27,'[31m',"Error",27,'[0m',": "


;; Symbols used for comparison
        
        static_symbol def_symbol, 'def!'
        static_symbol let_symbol, 'let*'
        
        static core_add_symbol, db "+"  
        static core_sub_symbol, db "-"
        static core_mul_symbol, db "*"
        static core_div_symbol, db "/"
        
section .text

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
.not_int:
        jmp quit
        
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
        call map_set
%endmacro

        
        
;; Takes a string as input and processes it into a form
read:
        jmp read_str           ; In reader.asm

;; ----------------------------------------------
;; Evaluates a form
;;
;; Inputs: RSI   Form to evaluate
;; 
eval_ast:
        ; Check the type
        mov al, BYTE [rsi]

        ; Check if this is a list
        mov ah, al
        and ah, container_mask
        cmp ah, container_list
        je .list
        
        cmp ah, container_map
        je .map
        
        cmp ah, container_vector
        je .vector
        
        ; Not a list, map or vector
        cmp ah, container_symbol
        je .symbol
        
        ; Not a symbol, list, map or vector
        call incref_object      ; Increment reference count
        
        mov rax, rsi
        ret
        
.symbol:
        ; look in environment
        mov rdi, rsi            ; symbol is the key
        mov rsi, [repl_env]     ; Environment
        call map_get
        je .done                ; result in RAX
        
        ; Not found, should raise an error
        
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret
        
.list:
        ; Evaluate each element of the list
        ;        
        xor r8, r8              ; The list to return
        ; r9 contains head of list

.list_loop:
        mov al, BYTE [rsi]      ; Check type
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        je .list_pointer
        
        ; A value in RSI, so copy
        
        call alloc_cons
        mov bl, BYTE [rsi]
        and bl, content_mask
        add bl, (block_cons + container_list)
        mov [rax], BYTE bl      ; set type
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx ; copy value

        ; Result in RAX
        jmp .list_append
        
.list_pointer:
        ; List element is a pointer to something
        push rsi
        push r8
        push r9
        mov rsi, [rsi + Cons.car] ; Get the address
        call eval             ; Evaluate it, result in rax
        pop r9
        pop r8
        pop rsi
        
        ; Check the type it's evaluated to
        mov bl, BYTE [rax]
        mov bh, bl
        and bh, (block_mask + container_mask)
        cmp bh, (block_cons + container_value)
        je .list_append
        
        ; Not a value, so need a pointer to it
        push rax
        call alloc_cons
        mov [rax], BYTE (block_cons + container_list + content_pointer)
        pop rbx                 ; Address to point to
        mov [rax + Cons.car], rbx
        ; Fall through to .list_append
        
.list_append:
        ; In RAX
        
        cmp r8, 0               ; Check if this is the first
        je .list_first

        ; append to r9
        mov [r9 + Cons.cdr], rax
        mov [r9 + Cons.typecdr], BYTE content_pointer
        mov r9, rax
        jmp .list_next
        
.list_first:
        mov r8, rax
        mov r9, rax
        ; fall through to .list_next
        
.list_next:
        ; Check if there's another
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .list_done          ; finished list
        mov rsi, [rsi + Cons.cdr] ; next in list
        jmp .list_loop
        
.list_done:
        mov rax, r8            ; Return the list
        ret
        
        ; ---------------------
.map:
        ; Create a new map, evaluating all the values
        
        ; Check if the map is empty
        cmp al, maltype_empty_map
        jne .map_not_empty

        ; map empty. Just return it
        call incref_object
        mov rax, rsi
        ret
        
.map_not_empty:
        
        mov r10, rsi            ; input in R10
        xor r12, r12            ; New map in r12
        
        ; Now loop through each key-value pair
        ; NOTE: This method relies on the implementation
        ; of map as a list    

.map_loop:
        ; Copy the key
        call alloc_cons         ; New Cons in RAX

        mov bl, [r10 + Cons.typecar] ; Type in BL
        mov [rax + Cons.typecar], bl
        mov rcx, [r10 + Cons.car] ; Value in RCX
        mov [rax + Cons.car], rcx

        ; Check the type of the key
        and bl, content_mask
        cmp bl, content_pointer
        jne .map_got_key        ; a value

        ; a pointer, so increment reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.map_got_key:
        cmp r12,0
        jne .append_key

        ; First key
        mov r12, rax
        mov r13, rax
        jmp .map_value
        
.append_key:
        ; Appending to previous value in r13
        mov [r13 + Cons.typecdr], BYTE content_pointer
        mov [r13 + Cons.cdr], rax
        mov r13, rax
        
.map_value:
        ; Check that we have a value
        mov al, BYTE [r10 + Cons.typecdr]
        cmp al, content_pointer
        jne .map_error_missing_value
        mov r10, [r10 + Cons.cdr]

        ; Now got value in r10

        ; Check the type of the value
        mov bl, [r10 + Cons.typecar] ; Type in BL
        and bl, content_mask
        cmp bl, content_pointer
        je .map_value_pointer

        ; Not a pointer, so make a copy
        call alloc_cons
        mov bl, [r10 + Cons.typecar]
        mov [rax + Cons.typecar], bl
        mov rcx, [r10 + Cons.car]
        mov [rax + Cons.car], rcx
        
        jmp .map_got_value
.map_value_pointer:
        ; A pointer, so need to evaluate
        push r10                ; Input
        push r12                ; start of result
        push r13                ; Current head of result
        mov rsi, [r10 + Cons.car] ; Get the address
        call eval               ; Evaluate it, result in rax
        pop r13
        pop r12
        pop r10
        
        ; Check the type it's evaluated to
        mov bl, BYTE [rax]
        mov bh, bl
        and bh, (block_mask + container_mask)
        cmp bh, (block_cons + container_value)
        
        jne .map_eval_pointer

        ; A value, so just change the type to a map
        and bl, content_mask
        add bl, (block_cons + container_map)
        mov [rax], BYTE bl
        jmp .map_got_value
        
.map_eval_pointer:
        ; Not a value, so need a pointer to it
        push rax
        call alloc_cons
        mov [rax], BYTE (block_cons + container_map + content_pointer)
        pop rbx                 ; Address to point to
        mov [rax + Cons.car], rbx
        
.map_got_value:
        ; Append RAX to list in R13
        mov [r13 + Cons.typecdr], BYTE content_pointer
        mov [r13 + Cons.cdr], rax
        mov r13, rax
        
        ; Check if there's another key
        mov al, BYTE [r10 + Cons.typecdr]
        cmp al, content_pointer
        jne .map_done          ; finished map
        mov r10, [r10 + Cons.cdr] ; next in map
        jmp .map_loop

.map_done:
        mov rax, r12
        ret
        
.map_error_missing_value:
        mov rax, r12
        ret
        
        ; ------------------------------
.vector:
        ; Evaluate each element of the vector
        ;        
        xor r8, r8              ; The vector to return
        ; r9 contains head of vector

.vector_loop:
        mov al, BYTE [rsi]      ; Check type
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        je .vector_pointer
        
        ; A value, so copy
        call alloc_cons
        mov bl, BYTE [rsi]
        and bl, content_mask
        add bl, (block_cons + container_vector)
        mov [rax], BYTE bl      ; set type
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx ; copy value

        ; Result in RAX
        jmp .vector_append
        
.vector_pointer:
        ; Vector element is a pointer to something
        push rsi
        push r8
        push r9
        mov rsi, [rsi + Cons.car] ; Get the address
        call eval             ; Evaluate it, result in rax
        pop r9
        pop r8
        pop rsi
        
        ; Check the type it's evaluated to
        mov bl, BYTE [rax]
        mov bh, bl
        and bh, (block_mask + container_mask)
        cmp bh, (block_cons + container_value)
        je .vector_append_value
        
        ; Not a value, so need a pointer to it
        push rax
        call alloc_cons
        mov [rax], BYTE (block_cons + container_vector + content_pointer)
        pop rbx                 ; Address to point to
        mov [rax + Cons.car], rbx
        jmp .vector_append

.vector_append_value:
        or bl, container_vector
        mov [rax], BYTE bl
        
.vector_append:
        ; In RAX
        
        cmp r8, 0               ; Check if this is the first
        je .vector_first

        ; append to r9
        mov [r9 + Cons.cdr], rax
        mov [r9 + Cons.typecdr], BYTE content_pointer
        mov r9, rax
        jmp .vector_next
        
.vector_first:
        mov r8, rax
        mov r9, rax
        ; fall through to .vector_next
        
.vector_next:
        ; Check if there's another
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .vector_done          ; finished vector
        mov rsi, [rsi + Cons.cdr] ; next in vector
        jmp .vector_loop
        
.vector_done:
        mov rax, r8            ; Return the vector
        ret
        
        ; ---------------------
.done:
        ret

;; ----------------------------------------------------
;; Evaluates a form
;;      
;; Input: RSI   AST to evaluate
;;
;; Returns: Result in RAX
;;
eval:
        ; Check type
        mov al, BYTE [rsi]
        cmp al, maltype_empty_list
        je .empty_list           ; empty list, return unchanged

        and al, container_mask
        cmp al, container_list
        je .list
        
        ; Not a list. Evaluate and return
        call eval_ast
        ret

        ; --------------------
.list:
        ; A list
        
        ; Check if the first element is a symbol
        mov al, BYTE [rsi]
        and al, content_mask
        cmp al, content_pointer
        jne .list_eval

        mov rbx, [rsi + Cons.car]
        mov al, BYTE [rbx]
        cmp al, maltype_symbol
        jne .list_eval
        
        ; Is a symbol, address in RBX
        push rsi

        ; Compare against def!
        mov rsi, rbx
        mov rdi, def_symbol
        call compare_char_array
        pop rsi
        cmp rax, 0
        je .def_symbol
        
        push rsi
        mov rdi, let_symbol
        call compare_char_array
        pop rsi
        cmp rax, 0
        je .let_symbol
        
        ; Unrecognised
        jmp .list_eval
        
.def_symbol:
        ; Define a new symbol in current environment

        jmp .list_not_function
.let_symbol:
        ; Create a new environment
        
        jmp .list_not_function
.list_eval:
        
        call eval_ast

        ; Check that the first element of the return is a function
        mov bl, BYTE [rax]
        and bl, content_mask
        cmp bl, content_pointer
        jne .list_not_function
        
        mov rbx, [rax + Cons.car] ; Get the address
        mov cl, BYTE [rbx]
        cmp cl, maltype_function
        jne .list_not_function
        
        ; Call the function with the rest of the list in RSI
        push rax
        mov rsi, [rax + Cons.cdr] ; Rest of list
        mov rdi, rbx ; Function object in RDI
        call [rbx + Cons.car]   ; Call function
        ; Result in rax
        pop rsi                 ; eval'ed list
        push rax
        call release_cons
        pop rax
        ret

.list_not_function:
        ; Not a function. Probably an error
        ret
        
.empty_list:
        mov rax, rsi
        ret
        
;; Prints the result
print:
        mov rdi, 1              ; print readably
        jmp pr_str

;; Read-Eval-Print in sequence
rep_seq:
        ; -------------
        ; Read
        call read
        push rax                ; Save form

        ; -------------
        ; Eval
        mov rsi, rax            ; Output of read into input of eval
        call eval
        push rax                ; Save result
        
        ; -------------
        ; Print

        mov rsi, rax            ; Output of eval into input of print
        call print              ; String in RAX

        mov r8, rax             ; Save output

        pop rsi                 ; Result from eval
        call release_object
        pop rsi                 ; Form returned by read
        call release_object
        mov rax, r8
        
        ret


_start:
        ; Create and print the core environment
        call map_new   ; Environment in RAX

        mov [repl_env], rax     ; store in memory

        mov rsi, rax            ; Environment

        ; Add +,-,*,/ to environment
        core_env_native core_add_symbol, core_add
        core_env_native core_sub_symbol, core_sub
        core_env_native core_mul_symbol, core_mul
        core_env_native core_div_symbol, core_div
        
        ; -----------------------------
        ; Main loop
        
.mainLoop:
        ; print the prompt
        print_str_mac prompt_string

        call read_line
        
        ; Check if we have a zero-length string
        cmp DWORD [rax+Array.length], 0
        je .mainLoopEnd

        push rax                ; Save address of the string

        mov rsi, rax
        call rep_seq            ; Read-Eval-Print

        push rax                ; Save returned string
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

        ; Release string from rep_seq
        pop rsi
        call release_array
        
        ; Release the input string
        pop rsi
        call release_array
        
        jmp .mainLoop
.mainLoopEnd:
        
        jmp quit
        
