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
%include "exceptions.asm"       ; Error handling

section .data

;; ------------------------------------------
;; Fixed strings for printing
        
        static prompt_string, db 10,"user> "      ; The string to print at the prompt
        
section .text

;; Takes a string as input and processes it into a form
read:
        jmp read_str           ; In reader.asm

;; ----------------------------------------------
;; Evaluates a form
;;
;; Inputs: RSI   Form to evaluate
;; 
eval:
        mov rax, rsi            ; Return the input
        ret
        
;; Prints the result
print:
        mov rdi, 1              ; print readably
        jmp pr_str

;; Read-Eval-Print in sequence
rep_seq:
        ; -------------
        ; Read
        call read
        push rax                ; Save form

        ; -------------
        ; Eval
        mov rsi, rax            ; Output of read into input of eval
        call eval
        
        ; -------------
        ; Print

        mov rsi, rax            ; Output of eval into input of print
        call print              ; String in RAX

        mov r8, rax             ; Save output
        pop rsi                 ; Form returned by read
        call release_object
        mov rax, r8
        
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
        
