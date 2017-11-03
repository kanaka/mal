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

;; Error handler list
error_handler: resq 1
        
section .data

;; ------------------------------------------
;; Fixed strings for printing
        
        static prompt_string, db 10,"user> "      ; The string to print at the prompt

        static error_string, db 27,'[31m',"Error",27,'[0m',": "

        static not_found_string, db " not found.",10

        static def_missing_arg_string, db "missing argument to def!",10

        static def_expecting_symbol_string, db "expecting symbol as first argument to def!",10

        static let_missing_bindings_string, db "let* missing bindings",10

        static let_bindings_list_string, db "let* expected a list of bindings",10

        static let_bind_symbol_string, db "let* expected a symbol in bindings list",10

        static let_bind_value_string, db "let* missing value in bindings list",10

        static let_missing_body_string, db "let* missing body",10


;; Symbols used for comparison
        
        static_symbol def_symbol, 'def!'
        static_symbol let_symbol, 'let*'
        
section .text   

;; ----------------------------------------------
;; 
;; Error handling
;;
;; A handler consists of:
;;  - A stack pointer address to reset to
;;  - An address to jump to
;;  - An optional data structure to pass
;;
;; When jumped to, an error handler will be given:
;;   - the object thrown in RSI
;;   - the optional data structure in RDI
;; 
        

;; Add an error handler to the front of the list
;;
;; Input: RSI - Stack pointer
;;        RDI - Address to jump to
;;        RCX - Data structure. Set to zero for none.
;;            If not zero, reference count incremented
;;
;; Modifies registers:
;;    RAX
;;    RBX
error_handler_push:
        call alloc_cons
        ; car will point to a list (stack, addr, data)
        ; cdr will point to the previous handler
        mov [rax], BYTE (block_cons + container_list + content_pointer)
        mov rbx, [error_handler]
        cmp rbx, 0              ; Check if previous handler was zero
        je .create_handler      ; Zero, so leave null
        ; Not zero, so create pointer to it
        mov [rax + Cons.typecdr], BYTE content_pointer
        mov [rax + Cons.cdr], rbx

        ; note: not incrementing reference count, since
        ; we're replacing one reference with another
.create_handler:
        mov [error_handler], rax ; new error handler
        
        mov rdx, rax
        call alloc_cons
        mov [rdx + Cons.car], rax
        ; Store stack pointer
        mov [rax], BYTE (block_cons + container_list + content_function)
        mov [rax + Cons.car], rsi ; stack pointer
        
        mov rdx, rax
        call alloc_cons
        mov [rdx + Cons.typecdr], BYTE content_pointer
        mov [rdx + Cons.cdr], rax
        ; Store function pointer to jump to 
        mov [rax], BYTE (block_cons + container_list + content_pointer)
        mov [rax + Cons.car], rdi

        ; Check if there is an object to pass to handler
        cmp rcx, 0
        je .done

        ; Set the final CDR to point to the object
        mov [rax + Cons.typecdr], BYTE content_pointer
        mov [rax + Cons.cdr], rcx

        mov rsi, rcx
        call incref_object
        
.done:
        ret
        
        
;; Removes an error handler from the list
;;
;; Modifies registers:
;;    RSI
;;    RAX
;;    RCX
error_handler_pop:
        ; get the address
        mov rsi, [error_handler]
        cmp rsi, 0
        je .done                ; Nothing to remove
        
        push rsi
        mov rsi, [rsi + Cons.cdr] ; next handler
        mov [error_handler], rsi
        call incref_object        ; needed because releasing soon
        
        pop rsi                 ; handler being removed
        call release_cons
        
.done:
        ret
        
        
;; Throw an error
;;   Object to pass to handler should be in RSI
error_throw:
        ; Get the next error handler
        mov rax, [error_handler]
        cmp rax, 0
        je .no_handler
        
        ; Got a handler
        mov rax, [rax + Cons.car] ; handler
        mov rbx, [rax + Cons.car] ; stack pointer
        mov rax, [rax + Cons.cdr]
        mov rcx, [rax + Cons.car] ; function
        mov rdi, [rax + Cons.cdr] ; data structure

        ; Reset stack
        mov rsp, rbx

        ; Jump to the handler
        jmp rcx
        
.no_handler:
        ; Print the object in RSI then quit
        cmp rsi, 0
        je .done                ; nothing to print
        call pr_str
        mov rsi, rax
        call print_string
.done:
        jmp quit_error
        
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
        push rsi
        xchg rsi, rdi
        ; symbol is the key in rdi
        ; Environment in rsi
        call env_get
        pop rsi
        je .done                ; result in RAX
        
        ; Not found, throw an error
        push rsi
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '

        pop rsi
        push rsi
        mov edx, [rsi + Array.length]
        add rsi, Array.data
        call print_rawstring    ; print symbol

        mov rsi, not_found_string
        mov rdx, not_found_string.len
        call print_rawstring    ; print ' not found'
        pop rsi

        jmp error_throw

        ; ------------------------------
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
        
        ; This may throw an error, so define a handler
        
        
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
        mov rsi, def_missing_arg_string
        mov rdx, def_missing_arg_string.len
        jmp .def_handle_error
        
.def_error_expecting_symbol:
        mov rsi, def_expecting_symbol_string
        mov rdx, def_expecting_symbol_string.len
        jmp .def_handle_error

.def_handle_error:
        push rsi
        push rdx
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '
        
        pop rdx
        pop rsi
        call print_rawstring    ; print message

        xor rsi, rsi            ; no object to throw
        jmp error_throw         ; No return
        
        ; -----------------------------
.let_symbol:
        ; Create a new environment

        mov r11, rsi            ; Let form in R11
        
        mov rsi, r15            ; Outer env
        call env_new
        mov r14, rax            ; New environment in R14
        
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

        ; Release the value
        mov rsi, rcx            ; The value
        call release_object
        
        ; Check if there are more bindings
        mov al, BYTE [r12 + Cons.typecdr]
        cmp al, content_pointer
        jne .let_done_binding
        mov r12, [r12 + Cons.cdr] ; Next
        jmp .let_bind_loop
        
.let_done_binding:
        ; Done bindings.
        ; Evaluate next item in let* form in new environment

        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .let_error_missing_body
        mov r11, [r11 + Cons.cdr] ; Now contains value to evaluate
        ; Check type of the value
        mov al, BYTE [r11]
        and al, block_mask + content_mask
        cmp al, content_pointer
        je .body_pointer

        ; Just a value, so copy
        call alloc_cons
        mov bl, BYTE [r11]
        and bl, content_mask
        mov [rax], BYTE bl      ; set type
        mov rbx, [r11 + Cons.car]
        mov [rax + Cons.car], rbx ; copy value
        jmp .let_done
        
.body_pointer:
        ; Evaluate using new environment
        
        mov rsi, r11
        mov rdi, r14            ; New environment
        push r14
        call eval
        pop r14
        
.let_done:
        ; Release the environment
        mov rsi, r14
        push rax
        call release_object
        pop rax
        ret
        
.let_error_missing_bindings:
        mov rsi, let_missing_bindings_string
        mov rdx, let_missing_bindings_string.len
        jmp .let_handle_error
        
.let_error_bindings_list:       ; expected a list, got something else
        mov rsi, let_bindings_list_string
        mov rdx, let_bindings_list_string.len
        jmp .let_handle_error
        
.let_error_bind_symbol:         ; expected a symbol, got something else
        mov rsi, let_bind_symbol_string
        mov rdx, let_bind_symbol_string.len
        jmp .let_handle_error
        
.let_error_bind_value:          ; Missing value in binding list
        mov rsi, let_bind_value_string
        mov rdx, let_bind_value_string.len
        jmp .let_handle_error
        
.let_error_missing_body:        ; Missing body to evaluate
        mov rsi, let_missing_body_string
        mov rdx, let_missing_body_string.len
        jmp .let_handle_error
        
.let_handle_error:
        push r11                ; For printing later
        
        push rsi
        push rdx
        mov rsi, error_string
        mov rdx, error_string.len
        call print_rawstring    ; print 'Error: '

        pop rdx
        pop rsi
        call print_rawstring    ; print message
        
        pop rsi                 ; let* form
        jmp error_throw         ; No return
        
        ; -----------------------------
        
.list_eval:

        mov rdi, r15            ; Environment
        push r15
        call eval_ast
        pop r15
        
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
        push r15
        mov rsi, [rax + Cons.cdr] ; Rest of list
        mov rdi, rbx ; Function object in RDI
        call [rbx + Cons.car]   ; Call function
        ; Result in rax
        pop r15
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

        ; Set the error handler
        mov rsi, rsp            ; Stack pointer
        mov rdi, .catch         ; Address to jump to
        xor rcx, rcx            ; No data
        call error_handler_push
        
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
        
.catch:
        ; Jumps here on error

        ; Check if an object was thrown
        cmp rsi, 0
        je .catch_done_print                ; nothing to print
        call pr_str
        mov rsi, rax
        call print_string
.catch_done_print:
        jmp .mainLoop           ; Go back to the prompt
        
