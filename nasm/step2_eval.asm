;; 
;; nasm -felf64 step1_read_print.asm && ld step1_read_print.o && ./a.out

;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
global  _start
        
%include "types.asm"            ; Data types, memory
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
  
section .text   
        
;; Evaluates a form in RSI
eval_ast:
        ; Check the type
        mov al, BYTE [rsi]

        ; Check if this is a list
        mov ah, al
        and ah, container_mask
        cmp ah, container_list
        je .list
        
        ;cmp ah, container_map
        ;je .map
        
        ; Not a list or a map
        cmp ah, container_symbol
        je .symbol
        
        ; Not a symbol, list or map
        call incref_object      ; Increment reference count
        
        mov rax, rsi
        ret
        
.symbol:
        ; look in environment
        mov rdi, rsi            ; symbol is the key
        mov rsi, [repl_env]     ; Environment
        call env_get
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
        mov r10, rsi            ; input in R10
        call map_keys           
        mov r11, rax            ; Get list of keys in R11
        mov r13, rax            ; Head of list in R13
        
        call map_new
        mov r12, rax            ; new map in R12

.map_loop:

        ; Check the type of the key
        mov al, BYTE [r13]
        and al, content_mask
        cmp al, content_pointer
        je .map_key_pointer

        mov [r13], BYTE al      ; Remove list container
        
        ; Get next value
        mov rsi, r10
        mov rdi, r13
        call map_get            ; Result in RAX

        ; Evaluate
        
        
        mov rsi, r12
        mov rdi, r13
        mov rcx, rax
        call map_set

        ; put back list container
        mov al, BYTE [r13]
        or al, container_list
        mov [r13], BYTE al
        
        jmp .map_next
        
.map_key_pointer:
        mov rsi, r10
        mov rdi, [r13 + Cons.car]
        call map_get            ; Result in RAX

        ; Evaluate
        
        mov rsi, r12
        mov rdi, [r13 + Cons.car]
        mov rcx, rax
        call map_set
        
.map_next:
        
        mov al, BYTE [r13 + Cons.typecdr]
        cmp al, content_pointer
        jne .map_done

        mov r13, [r13 + Cons.cdr] ; next key
        jmp .map_loop

.map_done:
        ; Release list of keys
        mov rsi, r11
        call release_cons
        
        mov rax, r12
        ret
        ; ---------------------
.done:
        ret

;; Evaluates a form in RSI
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
.list:
        ; A list
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
        
        mov rsi, rax
        call pr_str
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

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
        mov rsi, rax
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
        
