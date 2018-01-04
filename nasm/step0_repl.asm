;;
;; nasm -felf64 step0_repl.asm && ld step0_repl.o && ./a.out

;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
global  _start
        
%include "types.asm"            ; Data types, memory
%include "system.asm"           ; System calls
%include "printer.asm"          ; Data structures -> String
%include "exceptions.asm"       ; Error handling

section .data

;; ------------------------------------------
;; Fixed strings for printing
        
        static prompt_string, db 10,"user> "      ; The string to print at the prompt
        
section .text

;; Takes a string as input and processes it into a form
read:
        mov rax, rsi            ; Return the input
        ret

;; ----------------------------------------------
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
        ; -------------
        ; Read
        call read

        ; -------------
        ; Eval
        mov rsi, rax            ; Output of read into input of eval
        call eval
        
        ; -------------
        ; Print

        mov rsi, rax            ; Output of eval into input of print 
        call print
        
        ret


_start:
        
        ; -----------------------------
        ; Main loop
        
.mainLoop:
        ; print the prompt
        print_str_mac prompt_string

        call read_line
        
        ; Check if we have a zero-length string
        cmp DWORD [rax+Array.length], 0
        je .mainLoopEnd
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

        jmp .mainLoop
.mainLoopEnd:
        
        jmp quit
        
