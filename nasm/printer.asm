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
;; Modifies: RCX
;; Calls: raw_to_string, 
pr_str: 

        ; Get the type
        mov cl, BYTE [rsi]

        ; Check if it's already a string
        cmp cl, maltype_string
        jne .not_string
        mov rax, rsi
        ret
        
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
        ; Put '(' onto string
        mov rsi, rax
        mov cl, '('
        call string_append_char
        
        ; loop through list
        push rsi                ; Save output string
        
        ; Extract values and print
        ; mov bl, BYTE [r12]
        ; xor bl, container_list  ; Change from list to value
        ; mov BYTE [r12], bl
        ; mov rsi, r12

        mov rsi, r12
        mov BYTE [rsi], maltype_integer
        call pr_str             ; String in rax
        
        ; mov bl, BYTE [r12]
        ; xor bl, container_list  ; Change from value to list
        ; mov BYTE [r12], bl
        
        pop rsi                 ; Restore output string
        ; concatenate strings in rax and rsi
        mov rdx, rax            ; String to be copied

        push rax
        push rbx
        push rcx
        call string_append_string
        pop rcx
        pop rbx
        pop rax
        
        ; put ')' at the end of the string
        mov cl, ')'
        call string_append_char
        
        mov rax, rsi
        ret
.symbol:
        ret
        
