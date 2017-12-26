    
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

section .bss
        
;; Error handler list
error_handler: resq 1
        
section .text
        
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
        ; Note: This can't use content_pointer or release
        ; will try to release this memory address
        mov [rax], BYTE (block_cons + container_list + content_function)
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
        ;call incref_object        ; needed because releasing soon
        
        pop rsi                 ; handler being removed
        mov [rsi + Cons.typecdr], BYTE 0
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
        mov rdi, 1              ; print_readably
        call pr_str
        mov rsi, rax
        call print_string
.done:
        jmp quit_error
        
