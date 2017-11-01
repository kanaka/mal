      
;; ------------------------------------------------------------
;; Environment type
;; 
;; These are lists of maps. The head of the list is the
;; current environment, and CDR points to the outer environment
;;
;; ( {} {} ... )


section .data
        
env_symbol: ISTRUC Array
AT Array.type,  db   maltype_symbol
AT Array.length, dd  5
AT Array.data, db '*env*'
IEND

section .text

;; Create a new Environment
;;
;; Input: outer Environment in RSI. If zero, then nil outer.
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
        ret

;; Environment set
;;
;; Sets a key-value pair in an environment
;;
;; Inputs: RSI - env [not modified]
;;         RDI - key [not modified]
;;         RCX - value [not modified]
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

        
