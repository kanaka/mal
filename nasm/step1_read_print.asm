;; 
;; nasm -felf64 step1_read_print.asm && ld step1_read_print.o && ./a.out

;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
global  _start
        
%include "types.asm"            ; Data types, memory
%include "system.asm"           ; System calls
%include "reader.asm"           ; String -> Data structures
%include "printer.asm"          ; Data structures -> String
        
section .data

test_string1: db 10, "test1", 10
.len: equ $ - test_string1

test_string2: db 10, "test2", 10
.len: equ $ - test_string2    
        
;str: ISTRUC Array
;AT Array.type,  db   maltype_string
;AT Array.length, dd  6
;AT Array.data, db 'hello',10
;IEND

test_cons: ISTRUC Cons
AT Cons.typecar, db ( maltype_integer + 2 )
AT Cons.typecdr, db 0
AT Cons.car, dq 123
IEND

test_cons2: ISTRUC Cons
AT Cons.typecar, db ( maltype_integer + 2 )
AT Cons.typecdr, db content_pointer
AT Cons.car, dq 456
AT Cons.cdr, dq test_cons
IEND
        
;; ------------------------------------------
;; Fixed strings for printing
        
prompt_string: db 10,"user> "      ; The string to print at the prompt
.len: equ $ - prompt_string
  
section .text   
        
;; Evaluates a form
eval:
        mov rax, rsi            ; Return the input
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

        push rax                ; Save address of the string

        ; Put into read_str
        mov rsi, rax
        call read_str
        push rax
        
        ; Put into pr_str
        mov rsi, rax
        call pr_str
        push rax
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

        ; Release string from pr_str
        pop rsi
        call release_array
        
        ; Release the object from read_str
        pop rsi
        call release_object     ; Could be Cons or Array
        
        ; Release the string
        pop rsi
        call release_array
        
        jmp .mainLoop
.mainLoopEnd:
        
        jmp quit
        
