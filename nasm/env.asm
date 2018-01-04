
%include "macros.mac"
        
;; ------------------------------------------------------------
;; Environment type
;; 
;; These are lists of maps. The head of the list is the
;; current environment, and CDR points to the outer environment
;;
;; ( {} {} ... )
        
section .data

;; Symbols used for comparison
        static_symbol env_symbol, '*env*'
        static_symbol ampersand_symbol, '&'
        
;; Error message strings

        static env_binds_error_string, db "Env expecting symbol in binds list",10
        static env_binds_missing_string, db "Env missing expression in bind",10
        static env_missing_symbol_after_amp_string, db "Env missing symbol after &",10
        
section .text

;; Create a new Environment
;;
;; Input: outer Environment in RSI.
;;         - If zero, then nil outer.
;;         - If not zero, increments reference count
;;                   
;; Return a new Environment type in RAX
;;
;; Modifies registers:
;;   RAX
;;   RBX
env_new:
        call map_new            ; map in RAX
        push rax
        call alloc_cons         ; Cons in RAX
        pop rbx                 ; map in RBX

        mov [rax], BYTE (block_cons + container_list + content_pointer)
        ; CDR type already set to nil in alloc_cons
        mov [rax + Cons.car], rbx

        cmp rsi, 0
        jne .set_outer
        ret                     ; No outer, just return
.set_outer:
        mov [rax + Cons.typecdr], BYTE content_pointer
        mov [rax + Cons.cdr], rsi

        ; increment reference counter of outer
        mov rbx, rax            ; because incref_object modifies rax
        call incref_object
        mov rax, rbx
        ret

;; Create a new environment using a binding list
;;
;; Input: RSI - Outer environment
;;        RDI - Binds, a list of symbols
;;        RCX - Exprs, a list of values to bind each symbol to
;;
;; Modifies registers
;;    RBX
;;    RDX
;;    R8
;;    R9
;;    R10
;;    R11
;;    R12
;;    R13
env_new_bind:
        mov r11, rdi            ; binds list in R11
        mov r12, rcx            ; expr list in R12
        
        call env_new 
        mov r13, rax             ; New environment in R13
        
.bind_loop:
        ; Check the type in the bind list
        mov bl, BYTE [r11]
        and bl, content_mask
        cmp bl, content_empty
        je .done                ; No bindings
        
        cmp bl, content_pointer
        jne .bind_not_symbol

        mov rdi, [r11 + Cons.car] ; Symbol object?
        mov bl, BYTE [rdi]
        cmp bl, maltype_symbol
        jne .bind_not_symbol

        ; RDI now contains a symbol

        ; Check if it is '&'
        mov rsi, ampersand_symbol
        push rdi
        call compare_char_array ; Compares RSI and RDI
        pop rdi
        cmp rax, 0
        je .variadic            ; Bind rest of args to following symbol
        
        ; Check the type in expr

        mov bl, BYTE [r12]
        mov bh, bl
        and bh, content_mask

        cmp bh, content_empty
        je .bind_missing_expr   ; No expression
        
        cmp bh, content_pointer
        je .value_pointer
        
        ; A value. Need to remove the container type
        xchg bl,bh
        mov [r12], BYTE bl
        xchg bl,bh
        mov rcx, r12            ; Value
        mov rsi, r13            ; Env
        push rbx
        call env_set
        pop rbx
        ; Restore original type
        mov [r12], BYTE bl
        jmp .next

.value_pointer:
        ; A pointer to something, so just pass address to env_set
        mov rcx, [r12 + Cons.car]
        mov rsi, r13
        call env_set
        ; Fall through to next
.next:
        ; Check if there is a next symbol
        mov bl, BYTE [r11 + Cons.typecdr]
        cmp bl, content_pointer
        jne .done

        ; Got another symbol
        mov r11, [r11 + Cons.cdr] ; Next symbol

        ; Check if there's an expression to bind to
        mov bl, BYTE [r12 + Cons.typecdr]
        cmp bl, content_pointer
        jne .next_no_expr       ; No expr, but symbol could be &

        mov r12, [r12 + Cons.cdr] ; Next expression
        jmp .bind_loop
        
.next_no_expr:
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        mov r12, rax
        
        jmp .bind_loop
.done:
        mov rax, r13            ; Env
        ret

.variadic:
        ; R11 Cons contains '&' symbol
        ; Bind next symbol to the rest of the list in R12
        mov bl, BYTE [r11 + Cons.typecdr]
        cmp bl, content_pointer
        jne .missing_symbol_after_amp

        mov r11, [r11 + Cons.cdr]
        
        mov bl, BYTE [r11]
        and bl, content_mask
        cmp bl, content_pointer
        jne .bind_not_symbol
        
        mov rdi, [r11 + Cons.car] ; Symbol object?
        mov bl, BYTE [rdi]
        cmp bl, maltype_symbol
        jne .bind_not_symbol
        
        ; Bind symbol in RDI to R12
        mov rcx, r12            ; Value
        mov rsi, r13            ; Env
        call env_set
        jmp .done
        
.missing_symbol_after_amp:
        push r12

        ; Release the environment
        mov rsi, r13
        call release_object
        
        print_str_mac error_string   ; print 'Error: '
        print_str_mac env_missing_symbol_after_amp_string
        pop rsi
        jmp error_throw
        
.bind_not_symbol:               ; Expecting a symbol
        push r11                ; Binds list

        ; Release the environment
        mov rsi, r13
        call release_object
        
        print_str_mac error_string   ; print 'Error: '
        
        print_str_mac env_binds_error_string
        
        pop rsi                 ; Throw binds list
        jmp error_throw

.bind_missing_expr:
        ; Have a symbol, but no expression.
        
        push r11                ; Binds list
        
        ; Release the environment
        mov rsi, r13
        call release_object
        
        print_str_mac error_string   ; print 'Error: '
        
        print_str_mac env_binds_missing_string
        
        pop rsi                 ; Throw binds list
        jmp error_throw

        
;; Environment set
;;
;; Sets a key-value pair in an environment
;;
;; Inputs: RSI - env [not modified]
;;         RDI - key [not modified]
;;         RCX - value [not modified]
;;
;; Increments reference counts of key and value
;; if pointers to them are created
;; 
;; Modifies registers:
;;   R8
;;   R9
;;   R10
env_set:
        push rsi
        ; Get the first CAR, which should be a map
        mov rsi, [rsi + Cons.car]
        call map_set
        pop rsi
        ret
        
;; Environment get
;; 
;; Get a value from an environment, incrementing the reference count
;; of the object returned
;;
;; Inputs: RSI - environment
;;         RDI - key
;;
;; Returns: If found, Zero Flag is set and address in RAX
;;          If not found, Zero Flag cleared
env_get:
        push rsi

        ; Check special variable *env*
        mov rsi, env_symbol
        call compare_char_array
        pop rsi
        cmp rax, 0
        jne .not_env_symbol

        ; Env symbol, so return this environment
        call incref_object
        lahf                    ; flags in AH
        or ah, 64          ; set zero flag
        sahf
        mov rax, rsi
        ret
        
.not_env_symbol:
        push rsi
        ; Get the map in CAR
        mov rsi, [rsi + Cons.car]
        call map_get
        pop rsi
        je .found

        ; Not found, so try outer

        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .not_found
        
        mov rsi, [rsi + Cons.cdr] ; outer 
        jmp env_get
.found:
        ret

.not_found:
        lahf                    ; flags in AH
        and ah, 255-64          ; clear zero flag
        sahf
        ret

        
