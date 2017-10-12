
        
section .text

;; Read a string into memory as a form (nested lists and atoms)
;; Note: In this implementation the tokenizer is not done separately
;;
;; Input: Address of string (char array) in RSI
;;
read_str:


;; Initialise the tokenizer
tokenizer_init:
        ; Put start of data array into rax
        mov rax, rsi
        add rax, Array.data
        ; Put end of data array into rbx
        mov ebx, [rsi + Array.length] ; Length of array, zero-extended
        add rbx, rax
        
        ret

;; Move onto the next chunk of the array
;; This is needed because strings are not stored in one
;; contiguous block of memory, but may use multiple Array
;; objects in a linked list
;;
;; If no chunks are left, then RAX = RBX
tokenizer_next_chunk:
        mov rax, [rsi + Array.next]
        cmp rax, 0
        je .no_more
        ; More chunks left
        mov rsi, rax
        call tokenizer_init
        ret
.no_more:
        ; No more chunks left. RAX is zero
        mov rbx, rax
        ret

;; Moves the next char into CL
;; If no more, puts 0 into CL
tokenizer_next_char:
        ; Check if we have reached the end of this chunk
        cmp rax, rbx
        jne .chars_remain

        ; Hit the end. See if there is another chunk
        call tokenizer_next_chunk
        cmp rax, rbx
        jne .chars_remain      ; Success, got another

        ; No more chunks
        mov cl, 0               ; Null char signals end
        ret
        
.chars_remain:
        mov cl, BYTE [rax]
        inc rax                 ; point to next byte
        ret
        
;; Get the next token
;; Token code is in CL register. Could be:
;; - 0 : Nil, finished
;; - Characters ()[]()'`~^@
;; - Pair '~@', represented by code 1
;; - A string: " in CL, and address in R8
tokenizer_next:
        
.next_char:
        ; Fetch the next char into CL
        call tokenizer_next_char
        
        cmp cl, 0
        je .found               ; End, no more tokens
        
        ; Here expect to have:
        ; - The current character in CL
        ; - Address of next data in rax
        ; - Address of data end in rbx

        ; Skip whitespace or commas
        cmp cl, ' '             ; Space
        je .next_char           
        cmp cl, ','             ; Comma
        je .next_char
        cmp cl, 9               ; Tab

        ; Special characters. These are returned in CL as-is
        cmp cl, '('
        je .found
        cmp cl, ')'
        je .found
        cmp cl, '['
        je .found
        cmp cl, ']'
        je .found
        cmp cl, '{'
        je .found
        cmp cl, '}'
        je .found
        cmp cl, 39              ; character '
        je .found
        cmp cl, 96              ; character `
        je .found
        cmp cl, '^'
        je .found
        cmp cl, '@'
        je .found
        cmp cl, '~'             ; Could be followed by '@'
        je .handle_tilde
        
        cmp cl, ';'             ; Start of a comment
        je .tokens_finished

        cmp cl, '"'             ; Opening string quotes
        jmp .handle_string

        ; Could be number or symbol

        
        
        
        ret

.handle_string:
        ; Get an array to put the string into

        ; save state of tokenizer
        push rsi
        push rax
        push rbx

        call alloc_array
        mov r8, rax             ; Address of array in r8
        mov [r8], BYTE maltype_string ; mark as a string
        
        ; restore state
        pop rbx
        pop rax
        pop rsi
        
        ; Put start of data array into r9
        mov r9, r8
        add r9, Array.data
        ; Put end of data array into r10
        mov r10d, [rsi + Array.length] ; Length of array, zero-extended
        add r10, r9
        
        ; Now read chars from input string and push into output
.string_loop:
        call tokenizer_next_char
        cmp cl, 0               ; End of characters
        je .error
        
        cmp cl, '"'             ; Finished
        je .found               ; Leave '"' in CL

        cmp cl, 92              ; Escape '\'
        jne .end_string_escape
        
        ; Current character is a '\'
        call tokenizer_next_char
        cmp cl, 0               ; End of characters
        je .error

        cmp cl, 'n'             ; \n, newline
        je .insert_newline

        ; Whatever is in cl is now put into string
        ; including '"'
        jmp .end_string_escape
        
.insert_newline:
        mov cl, 10
        jmp .end_string_escape
        
.end_string_escape:

        ; Put CL onto result array
        mov [r9], cl
        inc r9

        jmp .string_loop
        
        ret
        
.tokens_finished:
        mov cl, 0               ; End of tokens
        ret
        
.handle_tilde:
        ; Could have '~' or '~@'. Need to peek at the next char

        ; Push current state of the tokenizer
        push rsi
        push rax
        push rbx
        call tokenizer_next_char ; Next char in CL
        cmp cl, '@'
        jne .tilde_no_amp           ; Just '~', not '~@'
        ; Got '~@'
        mov cl, 1               ; Signals '~@'

        ; Discard old state by moving stack pointer
        add esp, 24             ; 3 * 8 bytes
        ret
        
.tilde_no_amp:
        ; Restore state of the tokenizer
        pop rbx
        pop rax
        pop rsi
        ; fall through to finished
        
.found:
        ret

.error:
        ret
        
;; -----------------------------
;; list(tokens) tokenizer (string)
;;
;; Input string address in RSI
;; Creates a list of tokens, returns address in RAX
tokenizer:

        ; Get a new 
        call alloc_array
        ret
