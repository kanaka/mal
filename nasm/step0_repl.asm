;; nasm -felf64 mal.asm && ld mal.o && ./a.out

;; Calling convention: Address of input is in RSI
;;                     Address of return value is in RAX
;;
        
;; Data structures
;; Memory management is done by having two fixed-size datatypes,
;; Cons and Array.
;;
;; Both Cons and Array have the following in common:
;; a type field at the start, a reference count, followed by data
;; [ type (8) | (8) | refs (16) | data ]
        
        
;; 
STRUC Cons
.typecar: RESB 1                ; Type information for car (8 bit)
.typecdr: RESB 1                ; Type information for cdr (8 bits)
.refcount: RESW 1                ; Number of references to this Cons (16 bit)
.car: RESQ 1                    ; First value (64 bit)
.cdr: RESQ 1                    ; Second value (64 bit)
.size:                          ; Total size of struc
ENDSTRUC


%define array_chunk_len  32    ; Number of 64-bit values which can be stored in a single chunk
        
STRUC Array
.type: RESB 1                    ; Type information (8 bits)
.control: RESB 1                 ; Control data (8 bits)
.refcount: RESW 1                ; Number of references to this Array (16 bit)
.length: RESD 1                  ; Number of elements in array (32 bit)
.next RESQ 1                     ; Pointer to the next chunk (64 bit)
.data: RESQ array_chunk_len      ; Data storage
.size:                           ; Total size of struc
ENDSTRUC

;; Type information
%define type_char 1              ; Character type
%define type_integer 2           ; Integer type
%define type_float 3             ; Floating point number
%define type_array  128          ; Last bit tests if array or cons

        
        
        global  _start

section .data

str: ISTRUC Array
AT Array.type,  db   type_char + type_array
AT Array.length, dd  6
AT Array.data, db 'hello',10
IEND

prompt_string: db 10,"user> "      ; The string to print at the prompt
.len: equ $ - prompt_string
        
error_msg_print_string: db "Error in print string",10
.len: equ $ - error_msg_print_string

section .bss
        
section .text

;; ------------------------------------------
;; Array alloc_array()
;;
;; Returns the address of an Array object in RAX
alloc_array:
        mov rax, str
        ret

;; -------------------------------------------
;; Prints a raw string to stdout
;; String address in rsi, string length in rdx
print_rawstring:
        push rax
        push rdi

        ; write(1, string, length)
        mov     rax, 1                  ; system call 1 is write
        mov     rdi, 1                  ; file handle 1 is stdout
        syscall
        
        pop rdi
        pop rax

        ret
        
;; ------------------------------------------
;; void print_string(char array)
;; Address of the char Array should be in RSI
print_string:
        ; Push registers we're going to use
        push rax
        push rdi
        push rdx
        push rsi
        
        ; Check that we have a char array
        mov al, [rsi]
        cmp al, type_char + type_array
        jne .error
        
        ; write(1, string, length)
        mov   edx,  [rsi + Array.length] ; number of bytes
        add   rsi, Array.data         ; address of raw string to output
        call print_rawstring
        
        ; Restore registers
        pop rsi
        pop rdx
        pop rdi
        pop rax
        
        ret
.error:
        ; An error occurred
        mov     rdx, error_msg_print_string.len ; number of bytes
        mov     rsi, error_msg_print_string        ; address of raw string to output
        call print_rawstring
        ; exit
        jmp quit_error        
        
;; ------------------------------------------
;; String itostring(Integer number)
;;
;; Converts an integer to a string (array of chars)
;;
;; Input in RAX
;; Return string address in RAX
itostring:
        ; Save registers to restore afterwards
        push    rcx
        push    rdx
        push    rsi
        push    rdi
        
        mov     rcx, 0          ; counter of how many bytes we need to print in the end
        
.divideLoop:
        inc     rcx             ; count each byte to print - number of characters
        xor     rdx, rdx
        mov     rsi, 10
        idiv    rsi             ; divide rax by rsi
        add     rdx, 48         ; convert rdx to it's ascii representation - rdx holds the remainder after a divide instruction
        ; Character is now in DL
        dec     rsp
        mov     BYTE [rsp], dl  ; Put onto stack
        
        cmp     rax, 0          ; can the integer be divided anymore?
        jnz     .divideLoop      ; jump if not zero to the label divideLoop

        ; Get an Array object to put the string into
        call alloc_array        ; Address in RAX
        
        ; put length into string
        mov     [rax + Array.length], ecx
        
        ; copy data from stack into string
        ; Note: Currently this does not handle long strings
        mov     rdi, rax
        add     rdi, Array.data ; Address where raw string will go
.copyLoop:
        mov BYTE dl, [rsp]      ; Copy one byte at a time. Could be more efficient
        mov [rdi], BYTE dl 
        inc rsp
        inc rdi
        dec rcx
        cmp rcx, 0
        jnz .copyLoop
        
        ; Restore registers
        pop     rdi
        pop     rsi
        pop     rdx
        pop     rcx
        
        ret

;; ----------------------------
;; int stringtoi(String)
;;
;; Convert a string (char array) to an integer
;;
;; Address of input string is in RSI
;; Output integer in RAX
stringtoi: 

        ret
        
;------------------------------------------
; void exit()
; Exit program and restore resources
quit:
        mov     eax, 60                 ; system call 60 is exit
        xor     rdi, rdi                ; exit code 0
        syscall                         ; invoke operating system to exit

quit_error:
        mov     eax, 60                 ; system call 60 is exit
        mov     rdi, 1                 ; exit code 1
        syscall


;; Takes a string as input and processes it into a form
read:
        mov rax, rsi            ; Return the input
        ret
        
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
        call read
        mov rsi, rax            ; Output of read into input of eval
        call eval
        mov rsi, rax            ; Output of eval into input of print 
        call print
        mov rsi, rax            ; Return value
        ret

;; Read a line from stdin
;; Gets a new string array, fills it until a newline or EOF is reached
;; Returns pointer to string in RAX
read_line:
        ; Get an array to put the string into
        ; Address in rax
        call alloc_array
        ; Mark it as a character array (string)
        mov BYTE [rax + Array.type], type_char + type_array

        push rax                ; Save pointer to string
        
        ; Read character by character until either newline or end of input
        mov ebx, 0              ; Count how many characters read
        mov rsi, rax
        add rsi, Array.data     ; Point to the data
.readLoop:
        mov  rax, 0             ; sys_read
        mov  rdi, 0             ; stdin
        mov  rdx, 1             ; count
        syscall

        ; Characters read in RAX
        cmp  rax, 0             ; end loop if read <= 0
        jle  .readLoopEnd

        mov cl, BYTE [rsi]
        
        cmp cl, 10      ; End if we read a newline
        je .readLoopEnd
        
        cmp cl, 8       ; Backspace?
        je .handleBackspace

        cmp cl, 31              ; Below space
        jle .readLoop           ; Ignore, keep going

        cmp cl, 127             ; DEL or above
        jge .readLoop           ; Ignore, keep going
        
        inc ebx
        inc rsi                 ; Move to next point in the array
        jmp .readLoop           ; Get another character
        
.handleBackspace:
        ; Check if we've read any characters
        cmp ebx, 0
        je .readLoop            ; If not, carry on the loop
        ; Characters have been read. Remove one
        dec ebx
        dec rsi
        jmp .readLoop
.readLoopEnd:
        pop rax                 ; Restore pointer to string
        mov DWORD [rax + Array.length], ebx ; Set string length
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
        
        mov rsi, rax            ; Put into input of print_string
        call print_string

        jmp .mainLoop
.mainLoopEnd:
        
        ;mov rdx, 1
        ;mov rsi, 
        ;call print_rawstring
        ;inc rsp                
        
        ;mov rax, 1223
        ;call itostring
        ;mov rsi, rax
        ;call print_string
        
        jmp quit
        
