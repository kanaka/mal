;; 
;; nasm -felf64 step5_tco.asm && ld step5_tco.o && ./a.out
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
%include "exceptions.asm"       ; Error handling
        
section .bss
        
;; Top-level (REPL) environment
repl_env:resq 1

section .data

;; ------------------------------------------
;; Fixed strings for printing
        
        static prompt_string, db 10,"user> "      ; The string to print at the prompt

        static error_string, db 27,'[31m',"Error",27,'[0m',": "

        static not_found_string, db " not found"

        static def_missing_arg_string, db "missing argument to def!",10

        static def_expecting_symbol_string, db "expecting symbol as first argument to def!",10

        static let_missing_bindings_string, db "let* missing bindings",10

        static let_bindings_list_string, db "let* expected a list or vector of bindings",10

        static let_bind_symbol_string, db "let* expected a symbol in bindings list",10

        static let_bind_value_string, db "let* missing value in bindings list",10

        static let_missing_body_string, db "let* missing body",10
        static eval_list_not_function, db "list does not begin with a function",10

        static if_missing_condition_string, db "missing condition in if expression",10

;; Symbols used for comparison
        
        static_symbol def_symbol, 'def!'
        static_symbol let_symbol, 'let*'
        static_symbol do_symbol, 'do'
        static_symbol if_symbol, 'if'
        static_symbol fn_symbol, 'fn*'
        
;; Startup string. This is evaluated on startup
        static mal_startup_string, db "(def! not (fn* (a) (if a false true)))"
        
        
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
        ; Check if first character of symbol is ':'
        mov al, BYTE [rsi + Array.data]
        cmp al, ':'
        je .keyword
        
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
        print_str_mac error_string ; print 'Error: ' 
        
        pop rsi
        push rsi
        mov edx, [rsi + Array.length]
        add rsi, Array.data
        call print_rawstring    ; print symbol
        
        print_str_mac not_found_string ; print ' not found'
        pop rsi

        jmp error_throw
        
        ; ------------------------------
        
.keyword:
        ; Just return keywords unaltered
        call incref_object
        mov rax, rsi
        ret
        
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
        push r15                  ; Env
        mov rdi, [rsi + Cons.car] ; Get the address
        mov rsi, r15

        call incref_object      ; Environment increment refs
        xchg rsi, rdi           ; Env in RDI, AST in RSI
        
        call incref_object      ; AST increment refs
        
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
        je .list_eval_value
        
        ; Not a value, so need a pointer to it
        push rax
        call alloc_cons
        mov [rax], BYTE (block_cons + container_list + content_pointer)
        pop rbx                 ; Address to point to
        mov [rax + Cons.car], rbx
        jmp .list_append
        
.list_eval_value:
        ; Got value in RAX, so copy
        push rax
        call alloc_cons         ; Copy in RAX
        pop rbx                 ; Value to copy in RBX
        mov cl, BYTE [rbx]
        and cl, content_mask
        or  cl, (block_cons + container_list)
        mov [rax], BYTE cl      ; set type
        mov rcx, [rbx + Cons.car]
        mov [rax + Cons.car], rcx ; copy value

        ; Release the value in RBX
        push rsi
        push rax
        mov rsi, rbx
        call release_cons
        pop rax
        pop rsi
        
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

        xchg rsi, rdi
        call incref_object      ; Environment increment refs
        xchg rsi, rdi

        call incref_object
        
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

        xchg rsi, rdi
        call incref_object      ; Environment increment refs
        xchg rsi, rdi

        call incref_object
        
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
        je .vector_eval_value
        
        ; Not a value, so need a pointer to it
        push rax
        call alloc_cons
        mov [rax], BYTE (block_cons + container_vector + content_pointer)
        pop rbx                 ; Address to point to
        mov [rax + Cons.car], rbx
        jmp .vector_append

.vector_eval_value:
        ; Got value in RAX, so copy
        push rax
        call alloc_cons         ; Copy in RAX
        pop rbx                 ; Value to copy in RBX
        mov cl, BYTE [rbx]
        and cl, content_mask
        or  cl, (block_cons + container_vector)
        mov [rax], BYTE cl      ; set type
        mov rcx, [rbx + Cons.car]
        mov [rax + Cons.car], rcx ; copy value

        ; Release the value in RBX
        push rsi
        push rax
        mov rsi, rbx
        call release_cons
        pop rax
        pop rsi
        
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


        
;; Comparison of symbols for eval function
;; Compares the symbol in RSI with specified symbol
;; Preserves RSI and RBX
;; Modifies RDI
%macro eval_cmp_symbol 1
        push rsi
        push rbx
        mov rsi, rbx
        mov rdi, %1
        call compare_char_array
        pop rbx
        pop rsi
        test rax, rax           ; ZF set if rax = 0 (equal)
%endmacro
        
;; ----------------------------------------------------
;; Evaluates a form
;;      
;; Input: RSI   AST to evaluate   [ Released ]
;;        RDI   Environment       [ Released ]
;;
;; Returns: Result in RAX
;;
;; Note: Both the form and environment will have their reference count
;; reduced by one (released). This is for tail call optimisation (Env),
;; quasiquote and macroexpand (AST)
;; 
eval:
        mov r15, rdi            ; Env

        push rsi                ; AST pushed, must be popped before return
        
        ; Check type
        mov al, BYTE [rsi]
        cmp al, maltype_empty_list
        je .empty_list           ; empty list, return unchanged

        and al, container_mask
        cmp al, container_list
        je .list
        
        ; Not a list. Evaluate and return
        call eval_ast
        jmp .return             ; Releases Env

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
        
        ; Compare against special form symbols
        
        eval_cmp_symbol def_symbol ; def!
        je .def_symbol
        
        eval_cmp_symbol let_symbol ; let*
        je .let_symbol

        eval_cmp_symbol do_symbol ; do
        je .do_symbol

        eval_cmp_symbol if_symbol ; if
        je .if_symbol

        eval_cmp_symbol fn_symbol ; fn
        je .fn_symbol
        
        ; Unrecognised
        jmp .list_eval

              
        ; -----------------------------
        
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

        xchg rsi, rdi
        call incref_object      ; Environment increment refs
        xchg rsi, rdi           ; since it will be decremented by eval

        call incref_object      ; AST increment refs
        
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

        mov rax, rcx
        jmp .return
       
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
        print_str_mac error_string   ; print 'Error: '
        
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
        call env_new            ; Increments R15's ref count
        mov r14, rax            ; New environment in R14

        mov rsi, r15
        call release_object     ; Decrement R15 ref count
        
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
        ; Can be either a list or vector
        cmp al, block_cons + container_list
        je .let_bind_loop
        cmp al, block_cons + container_vector
        je .let_bind_loop
        
        ; Not a list or vector
        jmp .let_error_bindings_list
        
.let_bind_loop:
        ; R12 now contains a list with an even number of items
        ; The first should be a symbol, then a value to evaluate
        
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

        mov rsi, r14
        call incref_object
        mov rdi, r14
        
        mov rsi, [r12 + Cons.car] ; Get the address
        
        call incref_object      ; Increment ref count of AST
        
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
        
        mov rsi, [r11 + Cons.car] ; Object pointed to
        call incref_object        ; will be released by eval
        
        mov r11, rsi              ; save new AST
        pop rsi                   ; Old AST
        call release_object
        mov rsi, r11            ; New AST
        
        mov rdi, r14            ; New environment

        jmp eval                ; Tail call
        ; Note: eval will release the new environment on return
        
.let_done:
        ; Release the new environment
        push rax
        mov rsi, r14
        call release_object
        pop rax

        ; Release the AST
        pop rsi
        push rax
        call release_object
        pop rax
        ret                     ; already released env
        
.let_error_missing_bindings:
        mov rsi, let_missing_bindings_string
        mov rdx, let_missing_bindings_string.len
        jmp .let_handle_error
        
.let_error_bindings_list:       ; expected a list or vector, got something else
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
        
        print_str_mac error_string   ; print 'Error: '

        pop rdx
        pop rsi
        call print_rawstring    ; print message
        
        pop rsi                 ; let* form
        jmp error_throw         ; No return
        
        ; -----------------------------

.do_symbol:
        mov r11, rsi            ; do form in RSI
        ; Environment in R15

        ; Check if there is a body
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .do_no_body         ; error

        mov r11, [r11 + Cons.cdr] ; Body in R11
        
.do_symbol_loop:

        ; Need to test if this is the last form
        ; so we can handle tail call
        
        mov bl, BYTE [r11 + Cons.typecdr]
        cmp bl, content_pointer
        jne .do_body_last ; Last expression

        ; not the last expression
        
        ; Check if this is a value or pointer
        mov al, BYTE [r11]
        and al, block_mask + content_mask
        cmp al, content_pointer
        jne .do_next            ; A value, so skip
        
        ; A pointer, so evaluate
        
        push r15
        push r11
        
        mov rsi, r15
        call incref_object      ; Increase Env reference
                                ; since eval will release Env
        
        mov rsi, [r11 + Cons.car]  ; Form
        call incref_object         ; Increment ref count since eval will release
        
        mov rdi, r15            ; Env
        call eval               ; Result in RAX
        
        ; Another form after this.
        ; Discard the result of the last eval
        mov rsi, rax
        call release_object
        
        pop r11
        pop r15
        
.do_next:
        mov r11, [r11 + Cons.cdr] ; Next in list
        
        jmp .do_symbol_loop
                
.do_body_last:
        ; The last form is in R11, which will be returned
        
        ; Check if this is a value or pointer
        mov al, BYTE [r11]
        and al, block_mask + content_mask
        cmp al, content_pointer
        jne .do_body_value_return
        jmp .do_body_expr_return

.do_body_value_return:
        ; Got a value as last form (in R11).
        ; Copy and return

        push rax                ; Type of value to return

        ; release Env
        mov rsi, r15
        call release_object

        ; Allocate a Cons object to hold value
        call alloc_cons
        pop rbx                 ; type in BL
        mov [rax], BYTE bl
        mov rbx, [r11 + Cons.car]
        mov [rax + Cons.car], rbx

        ; release the AST
        pop rsi
        mov r15, rax            ; not modified by release
        call release_object
        mov rax, r15
        
        ret

.do_body_expr_return:
        ; An expression to evaluate as the last form
        ; Tail call optimise, jumping to eval
        ; Don't increment Env reference count
        
        mov rsi, [r11 + Cons.car]  ; new AST form
        call incref_object         ; This will be released by eval

        mov r11, rsi              ; Save new AST
        pop rsi                   ; Remove old AST from stack
        call release_object
        mov rsi, r11
        
        mov rdi, r15            ; Env
        jmp eval                ; Tail call
        
.do_no_body:
        ; No expressions to evaluate. Return nil

        mov rsi, r15
        call release_object     ; Release Env

        ; release the AST
        pop rsi
        call release_object
        
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret
        
        ; -----------------------------
        
.if_symbol:
        mov r11, rsi            ; if form in R11
        ; Environment in R15

        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .if_no_condition
        
        mov r11, [r11 + Cons.cdr] ; Should be a condition

        ; Check if value or pointer
        mov al, BYTE [r11]
        and al, content_mask
        cmp al, content_pointer
        jne .if_cond_value

        ; A pointer, so evaluate

        push r15
        push r11

        mov rsi, r15
        call incref_object      ; Increase Env reference
        
        mov rsi, [r11 + Cons.car]  ; Form
        call incref_object         ; Increase Form/AST ref count
        
        mov rdi, r15            ; Env
        call eval               ; Result in RAX
        pop r11
        pop r15

        ; Get type of result
        mov bl, BYTE [rax]

        ; release value
        push rbx
        mov rsi, rax
        call release_object
        pop rbx

        ; Check type
        cmp bl, maltype_nil
        je .if_false
        cmp bl, maltype_false
        je .if_false
        
        jmp .if_true
        
.if_cond_value:

        ; A value
        cmp al, content_nil
        je .if_false
        cmp al, content_false
        je .if_false
        
        jmp .if_true

.if_false:
        ; Skip the next item
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .return_nil
        
        mov r11, [r11 + Cons.cdr]
        
.if_true:
        ; Get the next item in the list and evaluate it
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .return_nil         ; Nothing to return

        mov r11, [r11 + Cons.cdr]
        
        ; Check if value or pointer
        mov al, BYTE [r11]
        and al, content_mask
        cmp al, content_pointer
        je .if_got_pointer
        
.if_got_value:
        ; copy value in r11
        call alloc_cons
        mov bl, BYTE [r11]
        and bl, content_mask
        mov [rax], BYTE bl
        mov rbx, [r11 + Cons.car]
        mov [rax + Cons.car], rbx

        jmp .return
        
.if_got_pointer:
        mov rsi, [r11 + Cons.car]  ; Form
        call incref_object        ; Will be released by eval

        mov r11, rsi
        pop rsi
        call release_object     ; Release old AST
        mov rsi, r11            ; New AST
        
        mov rdi, r15            ; Env
        jmp eval                ; Tail call
        
.if_no_condition:               ; just (if) without a condition
        
        print_str_mac error_string
        print_str_mac if_missing_condition_string
        
        ; Release environment
        mov rsi, r15
        call release_object
        xor rsi, rsi            ; No object to throw
        jmp error_throw

.return_nil:
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil

.return:
        ; Release environment
        mov rsi, r15
        mov r15, rax            ; Save RAX (return value)
        call release_object

        ; Release the AST
        pop rsi                 ; Pushed at start of eval
        call release_object

        mov rax, r15            ; return value
        ret
        
        ; -----------------------------
        
.fn_symbol:
        mov r11, rsi            ; fn form in R11
        ; Environment in R15

        ; Get the binds and body of the function
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .fn_empty
        
        mov r11, [r11 + Cons.cdr]
        mov al, BYTE [r11]
        and al, content_mask
        cmp al, content_pointer
        jne .fn_binds_not_list
        
        mov r12, [r11 + Cons.car]  ; Should be binds list
        mov al, BYTE [r12]
        and al, (block_mask + container_mask)
        cmp al, (block_cons + container_list) 
        je .fn_got_binds        ; Can be list
        cmp al, (block_cons + container_vector)
        je .fn_got_binds        ; or vector
        jmp .fn_binds_not_list
        
.fn_got_binds:

        ; Next get the body of the function
        mov al, BYTE [r11 + Cons.typecdr]
        cmp al, content_pointer
        jne .fn_no_body

        mov r11, [r11 + Cons.cdr]
        ; Check value or pointer
        mov al, BYTE [r11]
        and al, content_mask
        cmp al, content_pointer
        jne .fn_is_value        ; Body in r11
        mov r11, [r11 + Cons.car]
        jmp .fn_got_body

.fn_is_value:
        ; Body is just a value, no expression
        mov [r11], BYTE al      ; Mark as value, not list
        
.fn_got_body:

        ; Now put into function type
        ;   Addr is "apply_fn", the address to call
        ;   Env in R15
        ;   Binds in R12
        ;   Body in R11

        call alloc_cons
        mov [rax], BYTE (block_cons + container_function + content_function)
        mov rbx, apply_fn
        mov [rax + Cons.car], rbx ; Address of apply function
        mov [rax + Cons.typecdr], BYTE content_pointer
        
        mov r13, rax            ; Return list in R13
        
        call alloc_cons
        mov [rax], BYTE (block_cons + container_function + content_pointer)
        mov [rax + Cons.car], r15 ; Environment
        mov [rax + Cons.typecdr], BYTE content_pointer
        
        mov [r13 + Cons.cdr], rax ; Append to list
        mov r14, rax              ; R14 contains last cons in list
        
        push rax
        mov rsi, r15
        call incref_object
        pop rax

        ; Binds
        
        call alloc_cons
        mov [rax], BYTE (block_cons + container_function + content_pointer)
        mov [rax + Cons.car], r12 ; Binds list
        mov [rax + Cons.typecdr], BYTE content_pointer
        
        mov [r14 + Cons.cdr], rax ; Append to list
        mov r14, rax

        push rax
        mov rsi, r12
        call incref_object
        pop rax
        
        call alloc_cons
        mov [rax], BYTE (block_cons + container_function + content_pointer)
        mov [rax + Cons.car], r11 ; Body of function
        
        mov [r14 + Cons.cdr], rax

        mov rsi, r11
        call incref_object
        
        mov rax, r13
        jmp .return
        
.fn_empty:
.fn_binds_not_list:
.fn_no_body:
        
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        jmp .return
        
        
        ; -----------------------------
        
.list_eval:
        push rsi
        mov rdi, r15            ; Environment
        push r15
        call eval_ast           ; List of evaluated forms in RAX
        pop r15
        pop rsi
        
.list_exec:
        ; This point can be called to run a function
        ; used by swap!
        ; 
        ; Inputs: RAX - List with function as first element
        ;               NOTE: This list is released
        ; 
        ; Check that the first element of the return is a function
        mov bl, BYTE [rax]
        and bl, content_mask
        cmp bl, content_pointer
        jne .list_not_function
        
        mov rbx, [rax + Cons.car] ; Get the address
        mov cl, BYTE [rbx]
        cmp cl, maltype_function
        jne .list_not_function

        ; Check the rest of the args
        mov cl, BYTE [rax + Cons.typecdr]
        cmp cl, content_pointer
        je .list_got_args
        
        ; No arguments
        push rbx                ; Function object
        push rax                ; List with function first

        ; Create an empty list for the arguments
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        mov rsi, rax            ; Argument list into RSI

        pop rax                 ; list, function first
        ;;  Put new empty list onto end of original list
        mov [rax + Cons.typecdr], BYTE content_pointer
        mov [rax + Cons.cdr], rsi
        
        pop rbx
        jmp  .list_function_call
.list_got_args:
        mov rsi, [rax + Cons.cdr] ; Rest of list
.list_function_call:
        ; Call the function with the rest of the list in RSI
        
        mov rdx, rax            ; List to release
        mov rdi, rbx ; Function object in RDI

        mov rbx, [rbx + Cons.car]   ; Call function
        cmp rbx, apply_fn
        je apply_fn_jmp             ; Jump to user function apply
        
        ; A built-in function, so call (no recursion)
        push rax
        push r15

        call rbx
        
        ; Result in rax
        pop r15
        pop rsi                 ; eval'ed list
        
        push rax
        call release_cons
        pop rax 
        jmp .return             ; Releases Env

.list_not_function:
        ; Not a function. Probably an error
        push rsi

        mov rsi, rax
        call release_object
        
        print_str_mac error_string
        print_str_mac eval_list_not_function
        pop rsi
        jmp error_throw

.empty_list:
        mov rax, rsi
        jmp .return

;; Applies a user-defined function
;;
;; Input: RSI - Arguments to bind
;;        RDI - Function object
;;        RDX - list to release after binding
;;        R15 - Env (will be released)
;;        R13 - AST released before return
;;
;; 
;; Output: Result in RAX
;;
;; This is jumped to from eval, so if it returns
;; then it will return to the caller of eval, not to eval
apply_fn_jmp:
        ; This is jumped to from eval with AST on the stack
        pop r13
apply_fn:
        push rsi
        ; Extract values from the list in RDI
        mov rax, [rdi + Cons.cdr]
        mov rsi, [rax + Cons.car] ; Env
        mov rax, [rax + Cons.cdr]
        mov rdi, [rax + Cons.car] ; Binds
        mov rax, [rax + Cons.cdr]
        mov rax, [rax + Cons.car] ; Body
        pop rcx                   ; Exprs
        
        ; Check the type of the body
        mov bl, BYTE [rax]
        and bl, block_mask + container_mask
        jnz .bind               
        ; Just a value (in RAX). No eval needed
        
        mov r14, rax            ; Save return value in R14
        
        mov rsi, rax
        call incref_object

        ; Release the list passed in RDX
        mov rsi, rdx
        call release_object

        ; Release the environment
        mov rsi, r15
        call release_object

        ; Release the AST
        mov rsi, r13
        call release_object
        
        mov rax, r14
        ret
.bind:
        ; Create a new environment, binding arguments
        push rax                ; Body
        
        mov r14, r13            ; Old AST. R13 used by env_new_bind
        
        push rdx
        call env_new_bind
        pop rdx

        mov rdi, rax       ; New environment in RDI

        ; Note: Need to increment the reference count
        ; of the function body before releasing anything,
        ; since if the function was defined in-place (lambda)
        ; then the body may be released early
        
        pop rsi            ; Body
        call incref_object ; Will be released by eval
        mov r8, rsi        ; Body in R8
        
        ; Release the list passed in RDX
        mov rsi, rdx
        call release_cons

        ; Release the environment
        mov rsi, r15
        call release_object

        ; Release the old AST
        mov rsi, r14
        call release_object

        mov rsi, r8             ; Body
        
        jmp eval           ; Tail call
        ; The new environment (in RDI) will be released by eval
        

;; Read-Eval-Print in sequence
;;
;; Input string in RSI
rep_seq:
        ; -------------
        ; Read
        call read_str
        
        ; -------------
        ; Eval
        mov rsi, rax            ; Form to evaluate
        mov rdi, [repl_env]     ; Environment

        xchg rsi, rdi
        call incref_object      ; Environment increment refs
        xchg rsi, rdi           ; since it will be decremented by eval
        
        call eval               ; This releases Env and Form/AST
        push rax                ; Save result of eval

        ; -------------
        ; Print

        mov rsi, rax            ; Output of eval into input of print
        mov rdi, 1              ; print readably
        call pr_str             ; String in RAX

        mov r8, rax             ; Save output

        pop rsi                 ; Result from eval
        call release_object
        mov rax, r8
        
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

        ; Evaluate the startup string

        mov rsi, mal_startup_string
        mov edx, mal_startup_string.len
        call raw_to_string      ; String in RAX
        
        push rax
        mov rsi, rax
        call read_str           ; AST in RAX
        pop rsi                 ; string

        push rax                ; AST
        call release_array      ; string
        pop rdi                 ; AST in RDI
        
        mov rsi, [repl_env]     ; Environment in RSI
        
        call incref_object      ; Environment increment refs
        xchg rsi, rdi           ; Env in RDI, AST in RSI
        
        call eval
        
        mov rsi, rax
        call release_object     ; Return from eval
        
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
        
.catch:
        ; Jumps here on error

        ; Check if an object was thrown
        cmp rsi, 0
        je .catch_done_print                ; nothing to print
        mov rdi, 1
        call pr_str
        mov rsi, rax
        call print_string
.catch_done_print:
        jmp .mainLoop           ; Go back to the prompt
        
