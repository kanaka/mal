;;; System call functions
;;; 
;;; This file contains system-specific functions,
;;; which use calls to the operating system (Linux)

section .data
        static error_open_file_string, db "Error opening file "
        static error_read_file_string, db "Error reading file "

section .bss
        
timespec:  RESQ 2 
        
section .text
        
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

;; Reads a file into a string
;;
;; Input: RSI - File name string (char Array)
;;
;; Returns: string in RAX
;;
;; Pieces from https://stackoverflow.com/questions/20133698/how-to-read-from-and-write-to-files-using-nasm-for-x86-64bit
read_file:

        mov rdi, rsi            ; Filename

        ; Need to add null terminator
        mov eax, DWORD [rdi + Array.length]
        cmp eax, (array_chunk_len * 8)
        je .error_filename      ; File name too long

        ; Insert a null terminator
        add rax, rdi
        mov [rax + Array.data], BYTE 0
        
        ; Open the file
        mov rax, 2
        add rdi, Array.data; filename in RDI
        xor rsi, rsi ; O_RDONLY in RSI
        syscall

        ; Check for error (return -1)
        cmp eax, 0
        jl .error_open
        
        mov rdi, rax ; File handle in RDI

        ; Create a string
        push rdi
        call string_new         ; In RAX
        pop rdi
        
        mov r9, rax             ; Current Array
        push rax                 ; This is popped in .done
.loop:
        ; Read next chunk
        push r9
        
        mov rsi, r9
        add rsi, Array.data      ; address
        
        mov rax, 0             ; sys_read
        ; file handle in RDI
        mov  rdx, (array_chunk_len * 8) ; count
        syscall

        pop r9
        
        ; Characters read in RAX
        
        cmp rax, 0
        jl .error_read
        
        cmp rax, (array_chunk_len * 8)
        jg .error_read
        
        mov [r9 + Array.length], DWORD eax
        
        jl .done

        ; May still be more to read.
        ; Allocate another
        call string_new
        mov [r9 + Array.next], rax
        mov r9, rax
        jmp .loop
        
.done:  
        ; Close the file
        mov rax, 3
        ;rdi = file handle
        syscall
        
        pop rax
        ret
        
.error_filename:    
.error_open:
        ; File name in RDI
        sub rdi, Array.data
        
        ; Make the error message
        mov rsi, error_open_file_string
        mov edx, error_open_file_string.len
        call raw_to_string
        mov rsi, rax
        mov cl, 39              ; (')
        call string_append_char
        mov rdx, rdi            ; file name
        call string_append_string
        mov cl, 39
        call string_append_char

        ; Error message in RSI
        jmp error_throw
        
.error_read:
        mov rsi, error_read_file_string
        mov edx, error_read_file_string.len
        call raw_to_string
        mov rsi, rax
        jmp error_throw


        
;; Returns the time in ms in RAX
clock_time_ms:
        mov rax, 228            ; clock_gettime
        mov rdi, 0              ; CLOCK_REALTIME
        mov rsi, timespec
        syscall
        
        mov rax, [timespec + 8] ; nanoseconds
        cqo                     ; Sign extend RAX into RDX
        mov rcx, 1000000
        idiv rcx                ; Divide RAX by 1e6 -> ms
        mov rbx, rax
        ; -> ms in RBX

        mov rax, [timespec]     ; Seconds
        mov rcx, 1000
        imul rcx               ; Convert to ms
        add rax, rbx          ; Add RBX
        
        ret
