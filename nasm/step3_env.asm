;; 
;; nasm -felf64 step3_env.asm && ld step3_env.o && ./a.out
;;
;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
global  _start
        
%include "types.asm"            ; Data types, memory
%include "env.asm"              ; Environment type
%include "system.asm"           ; System calls
%include "reader.asm"           ; String -> Data structures
%include "core.asm"             ; Core functions
%include "printer.asm"          ; Data structures -> String
        
section .bss
        
;; Top-level (REPL) environment
repl_env:resq 1
        
section .data

;; ------------------------------------------
;; Fixed strings for printing
        
prompt_string: db 10,"user> "      ; The string to print at the prompt
.len: equ $ - prompt_string

error_string: db 27,'[31m',"Error",27,'[0m',": "
.len: equ $ - error_string

not_found_string: db " not found.",10
.len: equ $ - not_found_string

def_missing_arg_string: db "missing argument to def!",10
.len: equ $ - def_missing_arg_string

def_expecting_symbol_string: db "expecting symbol as first argument to def!",10
.len: equ $ - def_expecting_symbol_string
        
def_symbol: ISTRUC Array
AT Array.type,  db   maltype_symbol
AT Array.length, dd  4
AT Array.data, db 'def!'
IEND
        
let_symbol: ISTRUC Array
AT Array.type,  db   maltype_symbol
AT Array.length, dd  4
AT Array.data, db 'let*'
IEND
        
section .text   

;; ----------------------------------------------
;; Evaluates a form
;;
;; Inputs: RSI   Form to evaluate
;;         RDI   Environment
;; 
eval_ast:
        mov r15, rdi             ; Save Env in r15
        
        ; Check the type
        mov al, BYTE [rsi]

        ; Check if this is a list
        mov ah, al
        and ah, container_mask
        cmp ah, container_list
        je .list
        
        cmp ah, container_map
        je .map
        
        ; Not a list or a map
        cmp ah, container_symbol
        je .symbol
        
        ; Not a symbol, list or map
        call incref_object      ; Increment reference count
        
        mov rax, rsi
        ret
        
.symbol:
        ; look in environment
        push rsi
        xchg rsi, rdi
        ; symbol is the key in rdi
        ; Environment in rsi
        call env_get
        pop rsi
        je .done                ; result in RAX
        
        ; Not found, should raise an error
        push rsi
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '

        pop rsi
        mov edx, [rsi + Array.length]
        add rsi, Array.data
        call print_rawstring    ; print symbol

        mov rsi, not_found_string
        mov rdx, not_found_string.len
        call print_rawstring    ; print ' not found'
        
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
        
        ; A value, so copy
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
        push r15                  ; Env
        mov rsi, [rsi + Cons.car] ; Get the address
        mov rdi, r15
        call eval             ; Evaluate it, result in rax
        pop r15
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
        push r15                ; Env
        mov rsi, [r10 + Cons.car] ; Get the address
        mov rdi, r15
        call eval               ; Evaluate it, result in rax
        pop r15
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
        
        ; ---------------------
.done:
        ret

;; ----------------------------------------------------
;; Evaluates a form
;;      
;; Input: RSI   Form to evaluate
;;        RDI   Environment
;;
;; Returns: Result in RAX
;;
eval:
        mov r15, rdi            ; Env
        
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
        push rbx
        
        ; Compare against def!
        mov rsi, rbx
        mov rdi, def_symbol
        call compare_char_array
        pop rbx
        pop rsi
        cmp rax, 0
        je .def_symbol
        
        push rsi
        push rbx
        mov rsi, rbx
        mov rdi, let_symbol
        call compare_char_array
        pop rbx
        pop rsi
        cmp rax, 0
        je .let_symbol
        
        ; Unrecognised
        jmp .list_eval
        
.def_symbol:
        ; Define a new symbol in current environment
                
        ; Next item should be a symbol
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .def_error_missing_arg
        mov rsi, [rsi + Cons.cdr]
        
        ; Now should have a symbol
        
        mov al, BYTE [rsi + Cons.typecar]
        and al, content_mask
        cmp al, content_pointer
        jne .def_error_expecting_symbol
        mov r8, [rsi + Cons.car] ; Symbol (?)

        mov al, BYTE [r8]
        cmp al, maltype_symbol
        jne .def_error_expecting_symbol

        ; R8 now contains a symbol
        
        ; expecting a value or pointer next
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .def_error_missing_arg
        mov rsi, [rsi + Cons.cdr]

        ; Check if this is a pointer
        mov al, BYTE [rsi]
        mov ah, al
        and ah, content_mask
        cmp ah, content_pointer
        je .def_pointer

        ; A value, so copy
        push rax
        call alloc_cons
        pop rbx                 ; BL now contains type
        and bl, content_mask
        add bl, (block_cons + container_value)
        mov [rax], BYTE bl
        mov rcx, [rsi + Cons.car]
        mov [rax + Cons.car], rcx
        mov rsi, rax
        
        jmp .def_got_value

.def_pointer:
        ; A pointer, so evaluate
        push r8                 ; the symbol
        push r15                ; Env
        mov rsi, [rsi + Cons.car] ; Pointer
        mov rdi, r15
        call eval
        mov rsi, rax
        pop r15
        pop r8
        
.def_got_value:
        ; Symbol in R8, value in RSI
        mov rdi, r8             ; key (symbol)
        mov rcx, rsi            ; Value
        mov rsi, r15            ; Environment
        call env_set
        
        mov rax, rcx            ; Return the value
        ret
       
.def_error_missing_arg:
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '

        mov rsi, def_missing_arg_string
        mov rdx, def_missing_arg_string.len
        call print_rawstring
        
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret
        
        
.def_error_expecting_symbol:
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '

        mov rsi, def_expecting_symbol_string
        mov rdx, def_expecting_symbol_string.len
        call print_rawstring
        
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

        ; -----------------------------
.let_symbol:
        ; Create a new environment

        mov r11, rsi
        
        mov rsi, r15     ; Outer env
        call env_new
        mov r14, rax            ; New environment in r14
        
        ; Second element should be the bindings
        
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .let_error_missing_bindings
        mov r11, [r11 + Cons.cdr]
        
        mov al, BYTE [r11]
        and al, content_mask
        cmp al, content_pointer
        jne .let_error_bindings_list
        
        mov r12, [r11 + Cons.car] ; should be bindings list
        mov al, BYTE [r12]
        and al, (block_mask + container_mask)
        cmp al, block_cons + container_list
        jne .let_error_bindings_list
        
        ; R12 now contains a list with an even number of items
        ; The first should be a symbol, then a value to evaluate

.let_bind_loop:

        ; Get the symbol
        mov al, BYTE [r12]
        and al, content_mask
        cmp al, content_pointer
        jne .let_error_bind_symbol

        mov r13, [r12 + Cons.car] ; Symbol (?)
        mov al, BYTE [r13]
        cmp al, maltype_symbol
        jne .let_error_bind_symbol

        ; R13 now contains a symbol to bind
        ; The next item in the bindings list (R12)
        ; should be a value or expression to evaluate

        mov al, BYTE [r12 + Cons.typecdr]
        and al, content_mask
        cmp al, content_pointer
        jne .let_error_bind_value
        mov r12, [r12 + Cons.cdr]
        
        ; got value in R12
        
        ; Check the type of the value
        mov bl, [r12 + Cons.typecar] ; Type in BL
        and bl, content_mask
        cmp bl, content_pointer
        je .let_value_pointer

        ; Not a pointer, so make a copy
        call alloc_cons
        mov bl, [r12 + Cons.typecar]
        and bl, content_mask
        ;or bl, (block_cons + container_value) ; 0
        mov [rax + Cons.typecar], bl
        mov rcx, [r12 + Cons.car]
        mov [rax + Cons.car], rcx

        jmp .let_got_value
        
.let_value_pointer:
        ; A pointer, so need to evaluate
        push r11                 ; let* form list
        push r12                 ; Position in bindings list
        push r13                 ; symbol to bind
        push r14                 ; new environment
        mov rsi, [r12 + Cons.car] ; Get the address
        mov rdi, r14
        call eval               ; Evaluate it, result in rax
        pop r14
        pop r13
        pop r12
        pop r11
        
.let_got_value:

        mov rsi, r14            ; Env
        mov rdi, r13            ; key
        mov rcx, rax            ; value
        call env_set
        
        ; Check if there are more bindings
        mov al, BYTE [r12 + Cons.typecdr]
        cmp al, content_pointer
        jne .let_done_binding
        mov r12, [r12 + Cons.cdr] ; Next
        jmp .let_bind_loop
        
.let_done_binding:
        ; Done bindings.
        ; Evaluate next item in let* form in new environment
        
        
        mov rax, r14
        ret
        
.let_error_missing_bindings:
.let_error_bindings_list:       ; expected a list, got something else
.let_error_bind_symbol:         ; expected a symbol, got something else
.let_error_bind_value:          ; Missing value in binding list
        
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret
        
        ; -----------------------------
        
.list_eval:

        mov rdi, r15            ; Environment
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
        mov rax, rsi            ; Return the input
        ret

;; Read-Eval-Print in sequence
rep_seq:
        call read_str
        mov rsi, rax            ; Output of read into input of eval
        call eval
        mov rsi, rax            ; Output of eval into input of print 
        call print
        mov rsi, rax            ; Return value
        ret


_start:
        ; Create and print the core environment
        call core_environment   ; Environment in RAX

        mov [repl_env], rax     ; store in memory
        
        ; -----------------------------
        ; Main loop
        
.mainLoop:
        ; print the prompt
        mov rdx, prompt_string.len ; number of bytes
        mov rsi, prompt_string        ; address of raw string to output
        call print_rawstring

        call read_line
        
        ; Check if we have a zero-length string
        cmp DWORD [rax+Array.length], 0
        je .mainLoopEnd

        push rax                ; Save address of the input string
        
        ; Put into read_str
        mov rsi, rax
        call read_str
        push rax                ; Save AST

        ; Eval
        mov rsi, rax            ; Form to evaluate
        mov rdi, [repl_env]     ; Environment
        call eval
        push rax                ; Save result
        
        ; Put into pr_str
        mov rsi, rax            
        call pr_str
        push rax                ; Save output string
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

        ; Release string from pr_str
        pop rsi
        call release_array

        ; Release result of eval
        pop rsi
        call release_object
        
        ; Release the object from read_str
        pop rsi
        call release_object     ; Could be Cons or Array
        
        ; Release the input string
        pop rsi
        call release_array
        
        jmp .mainLoop
.mainLoopEnd:
        
        jmp quit
        
