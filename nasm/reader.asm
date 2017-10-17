
        
section .text

;; Read a string into memory as a form (nested lists and atoms)
;; Note: In this implementation the tokenizer is not done separately
;;
;; Input: Address of string (char array) in RSI
;;
;; Output: Address of object in RAX
;;
;; Uses registers:
;;  R13  Address of the current list (starts 0)
;;  R14  Stack pointer at start. Used for unwinding on error
;;  R15  Address of first list. Used for unwinding on error
;;
read_str:
        ; Initialise tokenizer
        call tokenizer_init
        
        ; Get the next token
        call tokenizer_next

        ; Set current list to zero
        mov r13, 0

        ; Save stack pointer for unwinding
        mov r14, rsp
        
        ; check what type of token by testing CL
        cmp cl, 0
        jne .got_token
        
        ; No tokens. Return 'nil'
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret

.read_loop:

        call tokenizer_next
        cmp cl, 0
        jne .got_token

        ; Unexpected end of tokens
        
        mov rsp, r14            ; Restore stack
        mov rsi, r13            ; Top Cons
        call release_cons       ; This should delete everything
        
        call alloc_cons
        mov [rax], BYTE maltype_nil
        
        ret
        
.got_token:
        
        cmp cl, '('
        je .list_start
        
        cmp cl, ')'
        je .list_end

        cmp cl, 'i'
        je .append_object       ; Cons already in R8

        ; Unknown
        call alloc_cons
        mov [rax], BYTE maltype_nil
        ret
        
        ; --------------------------------
.list_start:
        ; Push current list onto stack
        push r13

        ; Push current state of the tokenizer
        push rsi
        push rax
        push rbx
        
        ; Start new list
        call alloc_cons         ; Address in rax

        mov [rax], BYTE (block_cons + container_list + content_nil)
        
        
        cmp r13, 0
        jne .list_link_last
        
        ; This is the top-level list
        mov r15, rax
        jmp .list_done
        
.list_link_last:
        ; The new list is nested
        mov [r13 + Cons.cdr], rax
        mov [r13 + Cons.typecdr], BYTE content_pointer
.list_done:
        mov r13, rax            ; Switch to new list
        
        ; Restore state
        pop rbx
        pop rax
        pop rsi

        jmp .read_loop

        ; --------------------------------
.list_end:

        ; Check if there is a list
        cmp r13, 0
        jne .list_end_ok

        call alloc_cons
        mov [rax], BYTE maltype_nil
        
        ret

.list_end_ok:
        
        ; Put the current list into r8
        mov r8, r13

        ; Pop the previous list
        pop r13
        
        jmp .append_object ; Add R8 to list in R13

        
        ; --------------------------------
.append_object:
        ; Append Cons in R8 to list in R13
        ; If no list in R13 (address is zero) then returns
        ; with R8 moved to RAX

        cmp r13, 0
        je .finished 

        ; Append to list        
        mov [r13 + Cons.cdr], r8
        mov [r13 + Cons.typecdr], BYTE content_pointer
        mov [r8 + Cons.typecdr], BYTE content_nil

        jmp .read_loop
        ; --------------------------------
.finished:
        ; No list to add this object to, so finished
        mov rax, r8
        ret

;; Initialise the tokenizer
;;
;; Input: Address of string in RSI
;; 
;; NOTE: This uses RSI, RAX and RBX, and expects these to be preserved
;; between calls to tokenizer_next_char
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
;; - An integer: 'i' in CL
;;
;; Address of object in R8
;; 
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

        cmp cl, 34             ; Opening string quotes
        je .handle_string

        ; Could be number or symbol

        ; Check for a character 0-9
        cmp cl, '0'
        jl .handle_symbol
        cmp cl, '9'
        jg .handle_symbol

.handle_integer:
        ; Start integer
        ; accumulate in EDX
        xor edx, edx

        ; Push current state of the tokenizer
        push rsi
        push rax
        push rbx
        
.integer_loop:
        ; Here have a char 0-9 in CL
        sub cl, '0'            ; Convert to number between 0 and 9
        movzx ebx, cl
        add edx, ebx

        ; Peek at next character
        push rdx
        call tokenizer_next_char ; Next char in CL
        pop rdx
        
        cmp cl, '0'
        jl .integer_finished
        cmp cl, '9'
        jg .integer_finished
        
        imul edx, 10
        
        jmp .integer_loop
        
.integer_finished:
        ; Next char not an int

        push rdx                ; Save the integer

        ; Get a Cons object to put the result into
        call alloc_cons
        ; Address of Cons now in RAX
        mov r8, rax
        mov [r8], BYTE maltype_integer

        pop rdx
        mov [r8 + Cons.car], rdx
        
        ; Restore state
        pop rbx
        pop rax
        pop rsi

        mov cl, 'i'             ; Mark as an integer
        ret

.handle_symbol:

        
        
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
        
        cmp cl, 34              ; Finishing '"'
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
        ; NOTE: this doesn't handle long strings (multiple memory blocks)
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
        add rsp, 24             ; 3 * 8 bytes
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
        
