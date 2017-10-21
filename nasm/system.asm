;;; System call functions
;;; 
;;; This file contains system-specific functions,
;;; which use calls to the operating system (Linux)

        
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

        
;; Read a line from stdin
;; Gets a new string array, fills it until a newline or EOF is reached
;; Returns pointer to string in RAX
read_line:
        ; Get an array to put the string into
        ; Address in rax
        call alloc_array
        ; Mark it as a character array (string)
        mov BYTE [rax + Array.type], maltype_string

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
