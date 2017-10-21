;;; Turns forms (lists, values/atoms) into strings
;;; 
;;;

section .data
unknown_type_string: db "#<UNKNOWN>"
.len: equ $ - unknown_type_string
        
unknown_value_string: db "#<UNKNOWN VALUE>"
.len: equ $ - unknown_value_string

nil_value_string: db "nil"
.len: equ $ - nil_value_string
        
section .text

;; Input: Address of object in RSI
;;
;; Output: Address of string in RAX
;;
;; Modifies:
;;  RCX
;;  R12
;;  R13
;; Calls: raw_to_string,
;;
;; 
pr_str: 

        ; Get the type
        mov cl, BYTE [rsi]
        
        ; Check if it's already a string
        cmp cl, maltype_string
        
        jne .not_string

        ; ---------------------------
        ; Handle string
        
        call string_new         ; Output string in rax
        
        mov r12, rax
        add r12, Array.data     ; Output data
        mov r13, rsi
        add r13, Array.data     ; Input data
        mov r14d, DWORD [rsi + Array.length]
        add r14, Array.data
        add r14, rsi            ; End of input data

        ; Put " at start of output string
        mov [r12], BYTE '"'
        inc r12

        ; Loop through the input string, escaping characters
        
.string_loop:
        cmp r13, r14
        je .string_finished
        
        mov cl, BYTE [r13]      ; Get next character
        inc r13
        
        cmp cl, '"'             ;
        je .string_escape_char

        cmp cl, 10              ; Newline
        je .string_newline

        ; No special case, just copy the byte
        mov [r12], BYTE cl
        inc r12
        jmp .string_loop
        
.string_newline:
        mov cl, 'n'
        ;jmp .string_escape_char
        
.string_escape_char:             ; Add a '\' before char in cl
        mov [r12], BYTE 92 ; Escape '\'
        inc r12
        mov [r12], BYTE cl
        inc r12
        jmp .string_loop

.string_finished:

        mov [r12], BYTE '"'     ; At the end
        inc r12
        ; Calculate length of string
        sub r12, rax
        sub r12, Array.data

        mov [rax + Array.length], DWORD r12d
        
        ret

        ; ----------------------------
.not_string:
        ; Now test the container type (value, list)
        
        mov ch, cl

        and ch, container_mask
        jz .value

        cmp ch, 2
        je .list

        cmp ch, 4
        je .symbol

        ; Unknown
        mov rsi, unknown_type_string
        mov edx, unknown_type_string.len
        call raw_to_string      ; Puts a String in RAX
        ret
        
.value:
        mov ch, cl
        and ch, content_mask
        jz .value_nil

        cmp ch, 48
        je .value_int

        mov rsi, unknown_value_string
        mov edx, unknown_value_string.len
        call raw_to_string      ; Puts a String in RAX
        ret
        
.value_nil:
        mov rsi, nil_value_string
        mov edx, nil_value_string.len
        call raw_to_string
        ret
        
.value_int:
        mov rax, [rsi + Cons.car]
        call itostring
        ret
.list:
        
        mov r12, rsi            ; Input list
        
        call string_new         ; String in rax
        mov r13, rax            ; Output string in r13
        
        ; Put '(' onto string
        mov rsi, rax
        mov cl, '('
        call string_append_char
        
        ; loop through list
.list_loop:
        
        ; Extract values and print
        
        mov rsi, r12
        mov cl, BYTE [rsi]      ; Get type

        ; Check if it's a pointer (address)
        mov ch, cl
        and ch, content_mask
        cmp ch, content_pointer
        je .list_loop_pointer

        cmp ch, content_empty
        je .list_check_end
        
        ; A value (nil, int etc. or function)
        xor cl, container_list  ; Remove list type -> value
        mov BYTE [rsi], cl

        push r13
        push r12
        call pr_str             ; String in rax
        pop r12
        pop r13
        
        mov cl, BYTE [r12]
        or cl, container_list  ; Restore list type
        mov  BYTE [r12], cl
        jmp .list_loop_got_str
.list_loop_pointer:
        mov rsi, [rsi + Cons.car] ; Address of object
        push r13
        push r12
        call pr_str             ; String in rax
        pop r12
        pop r13
        
.list_loop_got_str:
        ; concatenate strings in rax and rsi
        mov rsi, r13            ; Output string
        mov rdx, rax            ; String to be copied

        push rsi                ; Save output string
        push rax                ; save temporary string
        call string_append_string

        ; Release the string
        pop rsi                 ; Was in rax, temporary string
        call release_array

        pop rsi                 ; restore output string
.list_check_end:
        ; Check if this is the end of the list
        mov cl, BYTE [r12 + Cons.typecdr]
        cmp cl, content_nil
        je .list_finished

        ; More left in the list
        
        ; Add space between values
        mov cl, ' '
        mov rsi, r13
        call string_append_char
        
        ; Get next Cons
        mov r12, [r12 + Cons.cdr]
        jmp .list_loop
        
.list_finished:
        ; put ')' at the end of the string
        mov cl, ')'
        mov rsi, r13
        call string_append_char
        
        mov rax, rsi
        ret
.symbol:
        ; Make a copy of the string
        call string_new         ; in rax
        mov ebx, DWORD [rsi + Array.length]
        mov [rax + Array.length], ebx
        mov rcx, rsi
        add rcx, Array.data     ; Start of input data
        mov rdx, rsi
        add rdx, Array.size     ; End of input data
        mov r12, rax
        add r12, Array.data     ; Start of output data
.symbol_copy_loop:
        ; Copy [rax] -> [r12]
        mov rbx, [rcx]
        mov [r12], rbx
        add rcx, 8              ; Next 64 bits of input
        cmp rcx, rdx
        je .symbol_finished
        
        add r12, 8              ; Next 64 bits of output
        jmp .symbol_copy_loop
.symbol_finished:
        
        
        
        ret
        
