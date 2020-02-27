;; Data structures
;; ===============
;;
;; Memory management is done by having two fixed-size datatypes,
;; Cons and Array.
;;
;; Both Cons and Array have the following in common:
;; a type field at the start, a reference count, followed by data
;; [ type (8) | (8) | refs (16) | data ]
;;
;;
;; Type bit fields
;; ---------------
;;
;; The 8-bit type fields describe the Block, Container and Content type.
;;
;; The Block type is used for memory management, to determine the kind of memory block
;; The Container type indicates the data structure that the Cons or Array block is being used to represent
;; The Content type indicates the raw type of the data in the content
;;
;;  Block type [1 bit]: 
;;  0    0 - Cons memory block
;;  1    1 - Array memory block
;; 
;;  Container type [3 bits]:
;;  0    0 - Value (single boxed value for Cons blocks, multiple values for Array blocks).
;;  2    1 - List (value followed by pointer). Only for Cons blocks
;;  4    2 - Symbol (special char array). Only for Array blocks
;;  6    3 - Keyword. Only for Array blocks
;;  8    4 - Map
;; 10    5 - Function
;; 12    6 - Atom
;; 14    7 - Vector
;;
;;  Content type [4 bits]:
;;   0   0 - Nil
;;  16   1 - True
;;  32   2 - Char
;;  48   3 - Int
;;  64   4 - Float
;;  80   5 - Pointer (memory address)
;;  96   6 - Function (instruction address)
;; 112   7 - Empty (distinct from Nil)
;; 208   8 - False
;; 224   9 - Macro
;; 
;;
;; These represent MAL data types as follows:
;;
;; MAL type     Block     Container     Content
;; --------- | -------- | ---------- | ---------
;; integer      Cons       Value         Int    
;; symbol       Array      Symbol        Char
;; list         Cons       List          Any
;; vector       Cons       Vector        Any
;; nil          Cons       Value         Nil
;; true         Cons       Value         True
;; false        Cons       Value         False
;; string       Array      Value         Char
;; keyword      Array      Keyword       Char
;; hash-map     Cons       Map           Alternate key, values
;; atom         Cons       Atom          Pointer
;;

%include "macros.mac"
        
;; Cons type.
;; Used to store either a single value with type information
;; or a pair of (value, Pointer or Nil) to represent a list
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
.length: RESD 1                  ; Number of elements in this part of the array (32 bit)
.next RESQ 1                     ; Pointer to the next chunk (64 bit)
.data: RESQ array_chunk_len      ; Data storage
.size:                           ; Total size of struc
ENDSTRUC

;; Type information

%define block_mask 1       ; LSB for block type 
%define container_mask 2 + 4 + 8 ; Next three bits for container type
%define content_mask 16 + 32 + 64 + 128 ; Four bits for content type
        
;; Block types
%define block_cons  0           ; Note: This must be zero
%define block_array 1

;; Container types
%define container_value  0      ; Note: This must be zero
%define container_list 2
%define container_symbol 4
%define container_keyword 6
%define container_map 8
%define container_function 10
%define container_atom 12
%define container_vector 14
        
;; Content type
%define content_nil  0
%define content_true 16
%define content_char 32
%define content_int 48
%define content_float 64
%define content_pointer 80      ; Memory pointer (to Cons or Array)
%define content_function 96     ; Function pointer
%define content_empty 112
%define content_false 208
%define content_macro 224 
        
;; Common combinations for MAL types
%define maltype_integer  (block_cons + container_value + content_int)
%define maltype_string  (block_array + container_value + content_char)
%define maltype_symbol  (block_array + container_symbol + content_char)
%define maltype_nil  (block_cons + container_value + content_nil)
%define maltype_empty_list (block_cons + container_list + content_empty)
%define maltype_empty_map (block_cons + container_map + content_empty)
%define maltype_empty_vector (block_cons + container_vector + content_empty)
%define maltype_function (block_cons + container_function + content_function)
%define maltype_macro (block_cons + container_function + content_macro)
%define maltype_true (block_cons + container_value + content_true)
%define maltype_false (block_cons + container_value + content_false)
%define maltype_atom (block_cons + container_atom + content_pointer)
        
;; ------------------------------------------

section .data
        
;; Fixed strings for printing

        static error_msg_print_string, db "Error in print string",10
        static error_array_memory_limit,  db "Error: Run out of memory for Array objects. Increase heap_array_limit.",10
        static error_cons_memory_limit, db "Error: Run out of memory for Cons objects. Increase heap_cons_limit.",10

        static error_cons_double_free, db "Error: double free error releasing Cons"
        static error_array_double_free, db "Error: double free error releasing Array"
        
;; ------------------------------------------
;; Memory management
;;
;; For each object (Cons or Array), there is a block of memory (in BSS).
;; When an object is requested it is first taken from the free list
;; If the free list is empty (address 0) then the next object in the block
;; is used, and the heap_x_number counter is incremented. When an object
;; is free'd it is pushed onto the heap_x_free list.
        
        
%define heap_cons_limit 5000     ; Number of cons objects which can be created

heap_cons_next: dq  heap_cons_store  ; Address of next cons in memory
heap_cons_free: dq 0            ; Address of start of free list
        
%define heap_array_limit 2000     ; Number of array objects which can be created
        
heap_array_next: dq heap_array_store
heap_array_free: dq 0
        
section .bss

;; Reserve space to store Cons and Array objects
heap_cons_store: resb  heap_cons_limit * Cons.size
.end:                           ; Address of end of the store
        
heap_array_store: resb heap_array_limit * Array.size
.end: 
        
section .text
        
;; ------------------------------------------
;; Array alloc_array()
;;
;; Returns the address of an Array object in RAX
;;
;; Working registers: rbx
alloc_array:
        
        ; Get the address of a free array
        mov rax, [heap_array_free] ; Address of the array
        
        ; Check if it's null
        cmp rax, 0
        je .create_array
        
        mov rbx, [rax + Array.next] ; Get the address of the next array in the linked list
        mov [heap_array_free], rbx  ; Put this address at the front of the list
        jmp .initialise_array
        
.create_array:

        ; Get the address of the next Array
        mov rax, [heap_array_next]
        ; Check if we've reached the end
        cmp rax, heap_array_store.end
        je .out_of_memory

        mov rbx, rax
        add rbx, Array.size     ; Address of the next array
        mov [heap_array_next], rbx ; for next time
        
.initialise_array:
        ; Address of Array now in rax
        mov BYTE [rax + Array.type], block_array
        mov WORD [rax + Array.refcount], 1 ; Only one reference
        mov DWORD [rax + Array.length], 0
        mov QWORD [rax + Array.next], 0 ; null next address
        
        ret
        
.out_of_memory:
        mov rsi, error_array_memory_limit
        mov rdx, error_array_memory_limit.len
        call print_rawstring
        jmp quit_error


;; -------------------------------------------
;; Decrements the reference count of the array in RSI
;; If the count reaches zero then push the array
;; onto the free list
release_array:
        mov ax, WORD [rsi + Array.refcount]

        ; Check if reference count is already zero
        test ax,ax
        jz .double_free
        
        dec ax
        mov WORD [rsi + Array.refcount], ax
        jz .free                ; If the count reaches zero then put on free list
        ret
        
.free:
        ; Get the next field
        mov rbx, [rsi + Array.next]
        
        mov rax, [heap_array_free] ; Get the current head
        mov [rsi + Array.next], rax ; Put current head into the "next" field
        mov [heap_array_free], rsi  ; Push Array onto free list
        
        cmp rbx, 0
        jne .release_next          ; If there is another array, then need to release it
        
        ret
        
.release_next:
        ; release the next array
        mov rsi, rbx
        call release_array
        ret

.double_free:
        ret
        load_static error_cons_double_free
        call raw_to_string
        mov rsi, rax
        jmp error_throw
        
;; ------------------------------------------
;; Cons alloc_cons()
;;
;; Returns the address of a Cons object in RAX
;;
;; Modifies:
;;   RBX
alloc_cons:
        
        ; Get the address of a free cons
        mov rax, [heap_cons_free] ; Address of the cons
        
        ; Check if it's null
        cmp rax, 0
        je .create_cons
        
        mov rbx, [rax + Cons.cdr] ; Get the address of the next cons in the linked list
        mov [heap_cons_free], rbx  ; Put this address at the front of the list
        jmp .initialise_cons
        
.create_cons:

        ; Get the address of the next Cons
        mov rax, [heap_cons_next]
        ; Check if we've reached the end
        cmp rax, heap_cons_store.end
        je .out_of_memory

        mov rbx, rax
        add rbx, Cons.size     ; Address of the next cons
        mov [heap_cons_next], rbx ; for next time
        
.initialise_cons:
        ; Address of Cons now in rax
        mov BYTE [rax + Cons.typecar], 0
        mov BYTE [rax + Cons.typecdr], 0
        mov WORD [rax + Cons.refcount], 1 ; Only one reference
        mov QWORD [rax + Cons.car], 0 
        mov QWORD [rax + Cons.cdr], 0 
        ret
        
.out_of_memory:
        mov rsi, error_cons_memory_limit
        mov rdx, error_cons_memory_limit.len
        call print_rawstring
        jmp quit_error


;; -------------------------------------------
;; Decrements the reference count of the cons in RSI
;; If the count reaches zero then push the cons
;; onto the free list
;;
;; Modifies registers:
;;    RAX
;;    RBX
;;    RCX
;;    
release_cons:
        mov ax, WORD [rsi + Cons.refcount]

        ; Check if already released
        test ax,ax
        jz .double_free
        
        dec ax
        mov WORD [rsi + Cons.refcount], ax
        jz .free                ; If the count reaches zero then put on free list
        ret
        
.free:
        ; Get and push cdr onto stack
        mov rcx, [rsi + Cons.cdr]
        push rcx                ; Content of CDR
        push rsi                ; Original Cons object being released
        
        mov rax, [heap_cons_free] ; Get the current head
        mov [rsi + Cons.cdr], rax ; Put current head into the "cdr" field
        mov [heap_cons_free], rsi  ; Push Cons onto free list

        ; Check if the CAR needs to be released
        
        mov al, BYTE [rsi+Cons.typecar]
        and al, content_mask    ; Test content type
        cmp al, content_pointer
        jne .free_cdr           ; Jump if CAR not pointer

        ; CAR is a pointer to either a Cons or Array
        ; Get the address stored in CAR
        mov rsi, [rsi + Cons.car]
        call release_object
.free_cdr:
        pop rcx                 ; This was rsi, the original Cons
        pop rsi                 ; This was rcx, the original Cons.cdr

        ; Get the type from the original Cons
        mov al, BYTE [rcx+Cons.typecdr]
        and al, content_mask    ; Test content type
        cmp al, content_pointer
        jne .done
        
        call release_object
.done:
        ret

.double_free:                   ; Already released
        ret
        load_static error_cons_double_free
        call raw_to_string
        mov rsi, rax
        jmp error_throw

;; Releases either a Cons or Array
;; Address of object in RSI
;;
;; May modify:
;;    RAX
;;    RBX
;;    RCX
;; 
release_object:
        mov al, BYTE [rsi]          ; Get first byte
        and al, block_mask          ; Test block type
        cmp al, block_array         ; Test if it's an array
        je release_array
        jmp release_cons

;; Increment reference count of Cons or Array
;; Address of object in RSI
;;
;; This code makes use of the fact that the reference
;; count is in the same place in Cons and Array types
;;
;; Modifies
;;   RAX
incref_object:
        mov ax, WORD [rsi + Cons.refcount] ; Same for Array
        inc ax
        ; Check for overflow?
        mov [rsi + Cons.refcount], WORD ax
        ret

;; -------------------------------------------
;; Copying lists/vectors
;; This does a shallow copy, copying only the
;; top level of objects. Any objects pointed to are not copied
;;
;; Input: RSI - address of list/vector
;;
;; Returns: New list/vector in RAX, last Cons in RBX
;;
;; Modifies:
;;    RBX
;;    RCX
;;    RDX
;;    R8
;;    R9
;;    R10
;;
cons_seq_copy:
        push rsi                ; Restored at the end
        
        mov r8, rsi             ; Input in R8
        xor r9, r9              ; Head of list in R9, start in R10
.loop:
        ; Check the type
        mov cl, BYTE [r8]
        mov ch, cl
        and ch, block_mask
        jnz .not_seq            ; Not a Cons object
        
        call alloc_cons
        mov rdx, rax            ; New Cons in RDX
        mov [rdx], BYTE cl      ; Copy type in RCX
        mov rbx, [r8 + Cons.car]  ; Value in RBX
        mov [rdx + Cons.car], rbx ; Copy value
        
        and cl, content_mask
        cmp cl, content_pointer
        jne .copied
        
        ; A pointer, so increment the reference count
        mov rsi, rbx
        call incref_object
        
.copied:
        ; Check if this is the first
        test r9,r9
        jnz .append

        ; First Cons
        mov r9, rdx
        mov r10, rdx            ; Start of the list, will be returned
        jmp .next
        
.append:
        ; Appending to last Cons
        mov [r9 + Cons.cdr], rdx
        mov [r9 + Cons.typecdr], BYTE content_pointer
        ; Replace
        mov r9, rdx
        
.next:
        ; Check if there's another
        mov al, BYTE [r8 + Cons.typecdr]
        cmp al, content_pointer
        jne .done               ; No more
        ; Got another
        mov r8, [r8 + Cons.cdr]
        jmp .loop

.done:
        pop rsi                 ; Restore input
        mov rax, r10            ; Output list
        mov rbx, r9             ; Last Cons
        ret
        
.not_seq:
        xor rsi,rsi
        jmp error_throw
        
;; -------------------------------------------
;; String type
;;
;; Create a new string, address in RAX
;;
;; Modifies registers
;;     RBX
;; 
string_new:
        call alloc_array
        mov [rax], BYTE maltype_string
        mov DWORD [rax + Array.length], 0
        mov QWORD [rax + Array.next], 0
        ret

;; Convert a raw string to a String type
;;
;; Input: Address of raw string in RSI, length in EDX
;; Output: Address of string in RAX
;;
;; Modifies registers: R8,R9,RCX
;;
raw_to_string:
        ; Save registers to restore at the end
        push r10
        push r11
        
        push rsi
        push rdx
        call string_new         ; String now in RAX
        pop rdx
        pop rsi
        
        mov r8, rax
        add r8, Array.data      ; Address of string data
        mov r10, rax
        add r10, Array.size     ; End of the destination data
        mov r11, rax            ; First Array to return
        
        mov r9, rsi             ; Address of raw data
        mov ecx, edx            ; Count
        
.copy_loop:
        test ecx, ecx           ; Check if count is zero
        jz .done
        
        ; Copy one byte
        mov bl, BYTE [r9]
        mov [r8], BYTE bl

        ; Move the destination
        inc r8
        cmp r8, r10
        jne .dest_ok
        
        ; Hit the end. Set the length of the array
        mov [rax + Array.length], DWORD (array_chunk_len * 8)

        push rax                ; Last Array
        push rsi
        push rdx
        call string_new         ; String now in RAX
        pop rdx
        pop rsi
        pop rbx                 ; Last Array
        mov [rbx + Array.next], rax ; Point to new Array
        
        mov r8, rax
        add r8, Array.data      ; Address of string data
        mov r10, rax
        add r10, Array.size     ; End of the destination data
        
.dest_ok:
        
        inc r9
        dec ecx
        jmp .copy_loop
.done:
        ; Set the length of the destination array
        sub r8, Array.data
        sub r8, rax
        mov [rax + Array.length], DWORD r8d

        ; Move first Array into RAX
        mov rax, r11

        ; Restore registers
        pop r11
        pop r10
        
        ret

;; Convert a raw string to a symbol
;; 
;; Input: Address of raw string in RSI, length in EDX
;; Output: Address of string in RAX
;;
;; Modifies registers: R8,R9,RCX
raw_to_symbol:
        call raw_to_string
        ; set the content type
        mov [rax], BYTE (block_array + container_symbol + content_char)
        ret

;; Convert a NUL terminated C string to string
;;
;; Input: RSI - Address of string
;;
;; Returns: String in RAX
;;
;; Modifies:
;;   RBX
;;   RCX
;;   RDX
        
cstring_to_string:
        push rsi
        call string_new         ; in RAX
        pop rsi

        mov rbx, rax
        add rbx, Array.data     ; Start of output
        mov rcx, rax
        add rcx, Array.size     ; End of output
.loop:
        mov dl, BYTE [rsi]
        test dl, dl             ; Check if NUL (0)
        jz .done
        mov [rbx], BYTE dl
        inc rbx
        inc rsi
        jmp .loop
.done:
        sub rbx, rax
        sub rbx, Array.data
        ; rbx now contains the length
        mov [rax + Array.length], DWORD ebx
        ret
        
;; Appends a character to a string
;; Input: Address of string in RSI, character in CL
;;
;; Modifies
;;    RAX
string_append_char:
        push rsi
        ; Get the end of the string
.get_end:
        mov rax, [rsi + Array.next]
        test rax, rax
        jz .got_dest_end
        mov rsi, rax
        jmp .get_end
.got_dest_end:

        ; Check if this chunk is full
        mov eax, DWORD [rsi + Array.length]
        cmp eax, (array_chunk_len*8)
        jne .append
        
        ; full, need to allocate another
        call alloc_array
        mov [rsi + Array.next], rax
        mov rsi, rax
        xor eax, eax            ; Set length to zero
.append:
        inc eax
        mov DWORD [rsi + Array.length], eax
        dec eax
        add rax, rsi
        add rax, Array.data            ; End of data
        mov [rax], BYTE cl
        
        pop rsi                 ; Restore original value
        ret

;; Appends a string to the end of a string
;; 
;; Input:  String to be modified in RSI
;;         String to be copied in RDX
;;
;; Output: Modified string in RSI
;;
;; Working registers:
;;   rax   Array chunk for output (copied to)
;;   rbx   Array chunk for input (copied from)
;;   cl    Character being copied
;;   r8    Address of destination
;;   r9    Destination end address
;;   r10   Address of source 
;;   r11   Source end address
string_append_string:
        ; copy source Array address to rbx
        mov rbx, rdx
        
        ; source data address in r10
        mov r10, rbx
        add r10, Array.data     ; Start of the data

        ; source data end address in r11
        mov r11, r10
        mov r8d, DWORD [rbx + Array.length]
        add r11, r8

        test r8d, r8d
        jz .return              ; Appending zero-size array
        
        ; Find the end of the string in RSI
        ; and put the address of the Array object into rax
        mov rax, rsi
.find_string_end:
        mov r8, QWORD [rax + Array.next]
        test r8, r8             ; Next chunk is 0
        je .got_dest_end        ; so reached end
        
        mov rax, r8             ; Go to next chunk
        jmp .find_string_end
.got_dest_end:
        
        ; destination data address into r8
        mov r8, rax
        add r8, Array.data
        add r8d, DWORD [rax + Array.length]
        
        ; destination data end into r9
        mov r9, rax
        add r9, Array.size

        ; Check if we are at the end of the destination
        cmp r8, r9
        je .alloc_dest
        
.copy_loop:        
        ; Copy one byte from source to destination
        mov cl, BYTE [r10]
        mov BYTE [r8], cl

        ; move source to next byte
        inc r10
        ; Check if we've reached the end of this Array
        cmp r10, r11
        jne .source_ok
        
        ; have reached the end of the source Array
        mov rbx, QWORD [rbx + Array.next]     ; Get the next Array address
        test rbx, rbx           ; Test if it's null
        je .finished            ; No more, so we're done
        ; Move on to next Array object
        
        ; Get source address into r10
        mov r10, rbx
        add r10, Array.data     ; Start of the data

        ; Source end address
        mov r11d, DWORD [rbx + Array.length] ; Length of the array
        add r11, r10

        ;  Check if the next array is empty
        cmp r10, r11
        je .finished
        
.source_ok:

        ; Move destination to next byte
        inc r8
        ; Check if we've reached end of the Array
        cmp r8, r9
        jne .copy_loop          ; Next byte

.alloc_dest:
        ; Reached the end of the destination
        ; Need to allocate another Array
        push rax
        push rbx
        call alloc_array        ; New Array in rax
        mov r8, rax             ; copy to r8
        pop rbx
        pop rax

        ; Previous Array in rax.
        ; Add a reference to the new array and set length
        mov QWORD [rax + Array.next], r8
        mov DWORD [rax + Array.length], (Array.size - Array.data)
        mov rax, r8             ; new array
        add r8, Array.data      ; Start of data
        
        mov r9, rax
        add r9, Array.size
        jmp .copy_loop
        
.finished:
        ; Compare r8 (destination) with data start
        ; to get length of string
        sub r8, rax
        sub r8, Array.data
        inc r8
        ; r8 now contains length
        mov DWORD [rax + Array.length], r8d
.return:        
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
        cmp al, maltype_string
        jne .error

.print_chunk:
        ; write(1, string, length)
        push rsi
        mov   edx,  [rsi + Array.length] ; number of bytes
        add   rsi, Array.data         ; address of raw string to output
        call print_rawstring
        pop rsi
        
        ; Check if this is the end
        mov rsi, QWORD [rsi + Array.next]
        cmp rsi, 0
        jne .print_chunk        ; next chunk
        
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

;; Copy a string
;;
;; Input: RSI - String to copy
;;
;; Output: New string in RAX
;;
;; Modifies:
;;    RBX
;;    RCX
;;    RDX
;;    RSI
;;
string_copy:
        call string_new         ; new string in RAX
        
        push rsi
        push rax
        
        ; Get lengths
        mov ebx, DWORD [rsi + Array.length]
        mov [rax + Array.length], ebx

        ; Copy the whole block of data
        ; Not sure if this is quicker than copying byte-by-byte
        ; Could divide ebx by 8 (rounded up) to get the number
        ; of blocks needed
        
        add rsi, Array.data     ; Start of input data
        add rax, Array.data     ; Start of output data
        mov ecx, array_chunk_len ; Number of 64-bit chunks
        
.loop:
        mov rbx, QWORD [rsi]
        mov [rax], QWORD rbx
        add rsi, 8
        add rax, 8
        dec ecx
        jnz .loop
        
        pop rax
        pop rsi
        ; Now check if there's another block
        mov rsi, [rsi + Array.next]
        cmp rsi, 0
        jz .done                ; Result in RAX

        ; Another array chunk
        push rax                ; Save output
        
        call string_copy        ; Copy next chunk
        mov rbx, rax            ; The copy in RBX
        
        pop rax
        ; append
        mov [rax + Array.next], rbx
.done:
        ret
        
;; ------------------------------------------
;; String itostring(Integer number)
;;
;; Converts an integer to a string (array of chars)
;;
;; Input in RAX
;; Return string address in RAX
itostring:
        ; Save registers to restore afterwards
        push    rbx
        push    rcx
        push    rdx
        push    rsi
        push    rdi
        
        mov     rcx, 0          ; counter of how many bytes we need to print in the end

        mov rbx, rax            ; Original input
        
        ; Check if the number is negative
        cmp rax, 0
        jge .divideLoop

        ; a negative number. To get the '-' sign
        ; at the front the test is done again at the end
        ; using the value stored in rbx

        neg rax                 ; Make it positive
        
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

        ; Check if the value was negative (in rbx)
        cmp rbx, 0
        jge .create_string

        ; a negative number
        dec rsp
        mov     BYTE [rsp], '-'
        inc rcx

.create_string:
        ; Get an Array object to put the string into
        call string_new        ; Address in RAX
        
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
        pop     rbx
        
        ret

        
;; ------------------------------------------------------------
;; Object comparison
;;
;; These comparison functions take two objects
;; in RSI and RDI
;; and return a code (not an object) in RAX
;;
;;  RAX = 0    Objects are equal
;;        1    RSI object is greater than RDI
;;        2    RSI object is less than RDI
;;       -1    Different object types, or no ordering
;;
;; Note that the ordering of objects depends on the type
;;    strings  - Alphabetical
;;   
;; 
;;

;; Given an object in RSI, follows pointers
;; to return the value object in RAX
;;
;; Modifies registers:
;;   RCX
compare_get_value:
        mov cl, BYTE [rsi]
        mov ch, cl
        and ch, block_mask
        jnz .nop                ; Got an Array

        ; Here got Cons
        mov ch, cl
        and ch, content_mask
        cmp ch, content_pointer
        jne .nop                ; Not a pointer

        ; Got a pointer, so follow and return
        mov rax, [rsi + Cons.car]
        ret
.nop:
        mov rax, rsi
        ret

;; Compare two objects in RSI and RDI.
;; Note that this does not compare lists
;; but will just compare the first element
;;
;; Modifies registers
;;    RAX, RBX, RCX, RDX
;;
compare_objects:
        ; Get the value that RSI points to
        call compare_get_value
        mov rbx, rax            ; Save in RBX
        ; Get the value that RDI points to
        mov rsi, rdi
        call compare_get_value
        mov rdi, rax
        mov rsi, rbx

        ; now get types
        mov cl, BYTE [rsi]      ; Type of RSI
        mov bl, BYTE [rdi]      ; Type of RDI

        mov ch, cl
        mov bh, bl
        
        ; Don't care about container type
        and cl, block_mask + content_mask
        and bl, block_mask + content_mask
        
        cmp bl, cl              ; compare block and content
        jne .different_types

        ; Here the same block, content type
        ; May be different container (value/list, string/symbol)

        ; Need to distinguish between map and vector/list
        and ch, (block_mask + container_mask)
        and bh, (block_mask + container_mask)
        cmp ch, bh
        je .same_container
        ; if either is a map, then different types
        cmp ch, container_map
        je .different_types
        cmp bh, container_map
        je .different_types
        
.same_container:
        cmp bl, block_cons + content_nil
        je .objects_equal      ; nil

        cmp bl, block_array + content_char
        je compare_char_array      ; strings, symbols
        
        cmp bl, block_cons + content_int
        je .integers

        ; Unknown
        jmp .different_types
        
.integers:
        ; two Cons objects, both containing integers
        mov rbx, [rsi + Cons.car]
        cmp rbx, [rdi + Cons.car]
        je .objects_equal
        jl .rdi_greater
        jmp .rsi_greater
        
.objects_equal:
        mov rax, 0
        ret

.rsi_greater:                   ; rsi > rdi
        mov rax, 1
        ret
        
.rdi_greater:                   ; rdi > rsi
        mov rax, 2
        ret
        
.different_types:
        mov rax, -1
        ret
        

;; Recursively check objects, including lists
;; 
;; Inputs: Objects in RSI and RDI
;;
;; Sets ZF if equal, clears flag otherwise
compare_objects_rec:
        ; Compare rsi and rdi objects
        
        ; Check type
        mov al, BYTE [rsi]
        mov bl, BYTE [rdi]
        
        mov ah, al
        mov bh, bl
        
        ; Don't distinguish between [] and ()
        and ah, (block_mask + content_mask)
        and bh, (block_mask + content_mask)
        
        cmp ah, bh
        jne .false

        ; Need to distinguish between map and vector/list
        mov ah, al
        mov bh, bl
        
        and ah, (block_mask + container_mask)
        and bh, (block_mask + container_mask)
        cmp ah, bh
        je .same_container
        ; if either is a map, then different types
        cmp ah, container_map
        je .false
        cmp bh, container_map
        je .false
        
.same_container:
        
        ; Check the container type
        and bh, block_mask
        jnz .array
        
        ; Check if a pointer to something
        and al, content_mask
        cmp al, content_pointer
        je .pointer

        ; Get the values
        
        mov rbx, [rsi + Cons.car]
        mov rcx, [rdi + Cons.car]
        cmp rbx, rcx
        jne .false
        
        ; Value is the same, so get next
        jmp .next

.array:
        ; Comparing arrays
        
        ; Container type (symbol/string) does matter
        cmp al, bl
        jne .false
        
        call compare_char_array
        cmp rax, 0
        ret                     ; Array has no next
        
.pointer:

        mov rbx, [rsi + Cons.car]
        mov rcx, [rdi + Cons.car]
        cmp rbx, rcx
        je .next                ; Equal pointers
        
        push rsi
        push rdi
        ; Put the addresses to compare into RSI and RDI
        mov rsi, rbx
        mov rdi, rcx
        call compare_objects_rec
        pop rdi
        pop rsi
        jne .false
        ; fall through to .next
        
.next:
        ; Check if both have a 'cdr' pointer
        mov al, BYTE [rsi + Cons.typecdr]
        mov bl, BYTE [rdi + Cons.typecdr]
        
        cmp al, content_pointer
        je .rsi_has_next
        
        ; No next pointer in RSI
        cmp bl, content_pointer
        je .false               ; RDI has a next pointer

        ; Neither have a next pointer, so done
        jmp .true
        
.rsi_has_next:
        cmp bl, content_pointer
        jne .false              ; RDI has no next pointer
        
        ; Both have a next pointer, so keep going
        mov rsi, [rsi + Cons.cdr]
        mov rdi, [rdi + Cons.cdr]
        jmp compare_objects_rec
        
.false:
        lahf                    ; flags in AH
        and ah, 255-64          ; clear zero flag
        sahf
        ret
.true:
        lahf                    ; flags in AH
        or ah, 64               ; set zero flag
        sahf
        ret
        
;; Char array objects (strings, symbols, keywords) in RSI and RDI
;; Return code in RAX
;;
;; Modifies registers:
;;   RBX
;;   RCX
;;   RDX
compare_char_array:
        ; Check length
        mov eax, DWORD [rsi + Array.length]
        mov ebx, DWORD [rdi + Array.length]
        cmp eax, ebx
        jne .different

        ; same length

        cmp eax, 0
        je .equal               ; Both zero length
        
        mov rbx, rsi
        add rbx, Array.data
        mov rcx, rdi
        add rcx, Array.data
.compare_loop:
        ; get next character
        mov dl, BYTE [rbx]
        cmp dl, BYTE [rcx]
        jl .rdi_greater
        jg .rsi_greater

        ; this character is equal
        inc rbx
        inc rcx
        dec eax
        jnz .compare_loop       ; Next character

.equal:
        mov rax, 0
        ret
        
.rsi_greater:                   ; rsi > rdi
        mov rax, 1
        ret
        
.rdi_greater:                   ; rdi > rsi
        mov rax, 2
        ret
        
.different:
        mov rax, -1
        ret
        
;; ------------------------------------------------------------
;; Map type
;;
;; This uses a list (Cons type) to represent key-value pairs in
;; a single chain. The only map which consists of an odd number of Cons
;; objects is the empty map, created by map_new
map_new:
        call alloc_cons
        mov [rax], BYTE (block_cons + container_map + content_empty)
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

;; Copy map
;;
;; Input:  RSI - map
;;
;; Returns: new map in RAX
;;
;; Modifies:
;;    RAX, RBX, RCX, R13, R14, R15
;;
map_copy:
        mov r14, rsi
        
        call alloc_cons
        mov r15, rax            ; start of new map
        xor r13, r13
.loop:
        mov bl, BYTE [rsi]
        mov rcx, [rsi + Cons.car]
        mov [rax], BYTE bl      ; copy type
        mov [rax + Cons.car], rcx ; copy value

        and bl, content_mask
        cmp bl, content_pointer
        jne .set_cdr

        ; A pointer in CAR. Increase reference count
        mov bx, WORD [rcx + Cons.refcount]
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.set_cdr:
        test r13,r13
        jz .next
        
        ; R13 contains last Cons
        mov [r13 + Cons.typecdr], BYTE content_pointer
        mov [r13 + Cons.cdr], rax
.next:
        mov r13, rax

        ; Check if there's another Cons
        mov bl, BYTE [rsi + Cons.typecdr]
        cmp bl, content_pointer
        jne .done               ; no more
        
        mov rsi, [rsi + Cons.cdr] ; next
        call alloc_cons
        jmp .loop
.done:
        mov rax, r15
        mov rsi, r14
        ret
        
        
;; Add to map. Input is a list with an even number of values
;; as (key, value, key, value, ...)
;;
;; Inputs:
;;    RSI - Map to append to. This is not modified
;;    RDI - List to add to the map
;; Outputs:
;;    RAX - New map
;;
;; Modifies:
;;    RCX
map_add:
        ; Check type of input
        mov cl, BYTE [rsi]
        mov cl, ch
        and ch, block_mask + container_mask
        cmp ch, block_cons + container_map
        jne .error
        
        mov cl, BYTE [rdi]
        and cl, block_mask + container_mask
        cmp cl, block_cons + container_list
        jne .error
        
        xor r8, r8              ; Zero r8
        
.copy_input:
        ; Copy input list, changing container type
        call alloc_cons

        mov cl, BYTE [rdi]
        and cl, content_mask    ; Keep the content
        add cl, block_cons + container_map
        mov [rax], BYTE cl      ; Set type
        mov rcx, [rdi+Cons.car] ; Copy data
        mov [rax+Cons.car], rcx
        
        cmp cl, (block_cons + container_map + content_pointer)
        jne .copy_not_pointer

        ; Copying a pointer to data
        ; so need to increase the reference count
        mov bx, WORD [rcx + Cons.refcount] ; Same offset for Array
        inc bx
        mov [rcx + Cons.refcount], WORD bx
        
.copy_not_pointer:

        ; Check if this is the first object
        cmp r8, 0
        jnz .copy_not_first
        mov r8, rax             ; Save start of map to R8
        mov r9, rax             ; Last cons in R9
        jmp .copy_next
        
.copy_not_first:
        ; Append to R9
        mov [r9+Cons.cdr], rax
        mov [r9+Cons.typecdr], BYTE content_pointer

        ; Put new Cons in R9 as the latest in the list
        mov r9, rax

.copy_next:
        ; Check if we've reached the end
        mov cl, BYTE [rdi + Cons.typecdr]
        cmp cl, content_nil
        je .copy_finished

        ; Not yet. Get next Cons and keep going
        mov rdi, [rdi + Cons.cdr]
        jmp .copy_input
        
.copy_finished:
        ; Start of map in r8, end in r9
        
        ; Check if the original map is empty
        mov cl, [rsi]
        and cl, content_mask
        cmp cl, content_empty
        je .return

        ; Put old map on the end of the new map
        ; For now this avoids the need to overwrite
        ; values in the map, since a search will find
        ; the new values first. 

        mov [r9 + Cons.cdr], rsi
        mov [r9 + Cons.typecdr], BYTE content_pointer
        
        ; Increment reference count
        mov bx, WORD [rsi + Cons.refcount]
        inc bx
        mov [rsi + Cons.refcount], WORD bx
        
.return:
        mov rax, r8
        ret
        
.error:
        ; Return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

;; Find a key in a map
;;
;; Inputs: RSI - map  [ Modified ]
;;         RDI - key  [ Modified ]
;;
;; Outputs: RAX - Cons object containing value in CAR
;;
;; Modifies registers:
;;   RBX [compare_objects, alloc_cons]
;;   RCX [compare_objects]
;;
;; 
;; If value is found then the Zero Flag is set
;;
;; Examples:
;;     {a 1 b 2} find a ->  {1 b 2}
;;     {1 2 3 4} find a ->  {4}
map_find:
        mov al, BYTE [rsi]
        cmp al, maltype_empty_map
        je .not_found

.map_loop:
        ; compare RSI and RDI, ignoring differences in container
        push rsi
        push rdi
        call compare_objects
        pop rdi
        pop rsi
        
        ; rax is now zero if objects are equal
        cmp rax, 0
        je .found

        ; Move along two cons to the next key
        mov al, [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .error              ; Expecting value after key
        
        mov rsi, [rsi + Cons.cdr] ; Get value
        mov al, [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .not_found
        
        mov rsi, [rsi + Cons.cdr] ; Get next key
        
        jmp .map_loop           ; Test next key
        
.found:
        
        lahf                    ; flags in AH
        or ah, 64               ; set zero flag
        sahf
        
        ; key in rsi. Get next value
        mov al, [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .error              ; Expecting value after key
        
        mov rsi, [rsi + Cons.cdr]
        
        ; ; increment reference count
        ; mov ax, WORD [rsi + Cons.refcount]
        ; inc ax
        ; mov [rsi + Cons.refcount], WORD ax
        ; Put address in rax
        mov rax, rsi
        ret

.not_found:
        lahf                    ; flags in AH
        and ah, 255-64          ; clear zero flag
        sahf
        
        ; last cons in rsi
        ; increment reference count
        ; mov ax, WORD [rsi + Cons.refcount]
        ; inc ax
        ; mov [rsi + Cons.refcount], WORD ax
        ; Put address in rax
        mov rax, rsi
        
        ret

.error:
        
        lahf                    ; flags in AH
        and ah, 255-64          ; clear zero flag
        sahf
        
        ; return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret

;; Map set
;;
;; Sets a key-value pair in a map
;;
;; Inputs: RSI - map [not modified]
;;         RDI - key [not modified]
;;         RCX - value [not modified]
;;
;; If references are added to key or value,
;; then reference counts are incremented.
;; 
;; Modifies registers:
;;   R8
;;   R9
;;   R10
map_set:
        ; Save inputs in less volatile registers
        mov r8, rsi             ; map
        mov r9, rdi             ; key
        mov r10, rcx            ; value
        
        ; Find the key, to see if it already exists in the map
        call map_find           ; Cons object in RAX
        je .found_key

        ; Key not in map. RAX should be address of the last
        ; value in the map, or empty
        mov bl, BYTE [rax]
        cmp bl, maltype_empty_map
        je .set_key
        
        ; Append key
        push rax
        call alloc_cons         ; New Cons in rax
        pop rbx                 ; Last Cons in map

        ; append rax to rbx
        mov [rbx + Cons.typecdr], BYTE content_pointer
        mov [rbx + Cons.cdr], rax
        jmp .set_key            ; Put key into rax
        
.found_key:
        ; Key already in map, so replace value
        ; address in RAX
        
        ; check type of value already there
        mov bl, BYTE [rax]
        and bl, content_mask
        cmp bl, content_pointer
        jne .set_value          ; Not a pointer, just overwrite

        ; A pointer, so need to release
        mov rsi, [rax + Cons.car] ; Address of object
        push rax
        call release_object
        pop rax
        
        jmp .set_value          ; put value into Cons
        
.set_key:
        ; Put key (R9) in RAX

        ; Check the type of object
        mov bl, BYTE [r9]
        mov bh, bl
        and bh, block_mask
        jnz .set_key_pointer  ; Array, so point to it
        
        ; Here a Cons object
        mov bh, bl
        and bh, container_mask
        cmp bh, container_value
        jne .set_key_pointer  ; Not a simple value, so point to it
        
        ; A value, so copy
        mov rcx, [r9 + Cons.car]
        mov [rax + Cons.car], rcx

        ; Set the type
        and bl, content_mask
        or bl, (block_cons + container_map)
        mov [rax], BYTE bl

        jmp .set_key_done
        
.set_key_pointer:
        ; The key is a pointer
        
        mov [rax + Cons.car], r9
        mov [rax], BYTE (block_cons + container_map + content_pointer)
        ; Increment reference count
        mov bx, WORD [r9 + Cons.refcount]
        inc bx
        mov [r9 + Cons.refcount], bx
        ; fall through to .set_key_done
        
.set_key_done:
        ; Key in RAX. allocate and append a Cons for the value
        push rax
        call alloc_cons      ; value Cons in rax
        pop rbx                 ; key Cons
        ; append rax to rbx
        mov [rbx + Cons.typecdr], BYTE content_pointer
        mov [rbx + Cons.cdr], rax

        ; fall through to .set_value
        
        ; --------------------------------
.set_value:
        ; Set the value into the Cons at [rax]
        
        ; Check the type of object
        mov bl, BYTE [r10]
        mov bh, bl
        and bh, block_mask
        jnz .set_value_pointer  ; Array, so point to it

        ; Here a Cons object
        mov bh, bl
        and bh, container_mask
        cmp bh, container_value
        jne .set_value_pointer  ; Not a simple value, so point to it
        ; A value, so copy
        mov rcx, [r10 + Cons.car]
        mov [rax + Cons.car], rcx

        ; Set the type
        and bl, content_mask
        or bl, (block_cons + container_map)
        mov [rax], BYTE bl

        jmp .finished
        
.set_value_pointer:
        mov [rax + Cons.car], r10 ; Put address into CAR
        mov [rax], BYTE (block_cons + container_map + content_pointer) ; Mark as a pointer
        ; Increment reference count
        mov bx, WORD [r10 + Cons.refcount]
        inc bx
        mov [r10 + Cons.refcount], bx
        ; fall through to .finished
        
.finished:
        ; Restore inputs
        mov rsi, r8
        mov rdi, r9
        mov rcx, r10
        ret
        
;; Get a value from a map, incrementing the reference count
;; of the object returned
;;
;; Inputs: RSI - map
;;         RDI - key
;;
;; Returns: If found, Zero Flag is set and address in RAX
;;          If not found, Zero Flag cleared
;;
;; Modifies registers:
;;   RAX
;;   RBX
;;   RCX
;;   R8
;;   R9
map_get:
        ; Save inputs
        mov r8, rsi             ; map
        mov r9, rdi             ; key

        call map_find           ; Cons object in RAX
        je .found_key

        ; Not found
        
        mov rsi, r8
        mov rdi, r9
        
        lahf                    ; flags in AH
        and ah, 255-64          ; clear zero flag
        sahf
        
        ret
        ; ---------------
.found_key:
        
        ; Check if the object in RAX is a value or pointer
        mov bl, BYTE [rax]
        and bl, content_mask
        cmp bl, content_pointer
        je .got_pointer

        ; A value, so copy

        push rax
        push rbx
        call alloc_cons         ; cons in rax
        pop rbx                 ; content type in bl
        pop rcx                 ; Object to copy
        
        add bl, block_cons + container_value
        mov [rax], BYTE bl      ; set type
        mov [rax + Cons.typecdr], BYTE content_nil

        ; Copy value
        mov rbx, [rcx + Cons.car]
        mov [rax + Cons.car], rbx

        jmp .finished_found
        
.got_pointer:
        ; A pointer, so get the address
        mov rax, [rax + Cons.car]

        ; increment reference count
        mov bx, WORD [rax + Cons.refcount]
        inc bx
        mov [rax + Cons.refcount], bx

        ; Fall through to .finished_found
.finished_found:
        mov rsi, r8
        mov rdi, r9

        mov rbx, rax
        lahf                    ; flags in AH
        or ah, 64               ; set zero flag
        sahf
        mov rax, rbx
        ret

;; Get a list of keys
;; 
;; Input: Map in RSI
;;
;; Returns: List in RAX
;;
;; Modifies registers:
;;   RAX
;;   RBX
;;   RCX
;;   R8
;;   R9
map_keys:
        ; check type
        mov al, BYTE [rsi]
        cmp al, maltype_empty_map
        je .empty_map

        and al, container_mask
        cmp al, container_map
        jne .empty_map          ; error
        
        xor r8, r8              ; Return list
        
        ; Take the current value
.loop:
        ; Create a new Cons for this key
        call alloc_cons
        mov cl, BYTE [rsi]
        and cl, content_mask
        add cl, block_cons + container_list
        mov [rax], BYTE cl      ; Set type
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx          ; Set value

        and cl, content_mask
        cmp cl, content_pointer
        jne .append

        ; A pointer, so increment reference count
        mov cx, WORD [rbx + Cons.refcount]
        inc cx
        mov [rbx + Cons.refcount], WORD cx
        
.append:
        cmp r8, 0
        je .first

        ; appending
        mov [r9 + Cons.typecdr], BYTE content_pointer
        mov [r9 + Cons.cdr], rax
        mov r9, rax
        jmp .next
.first:
        ; First key, so put into r8
        mov r8, rax
        mov r9, rax
.next:
        ; First get the value
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done              ; error. Should be a value
        mov rsi, [rsi + Cons.cdr]

        ; Get the next key
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done
        mov rsi, [rsi + Cons.cdr]
        jmp .loop
.done:
        ; Finished, return the list
        mov rax, r8
        ret
        
.empty_map:
        ; return empty list
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret

;; Get a list of values
;; 
;; Input: Map in RSI
;;
;; Returns: List in RAX
;;
;; Modifies registers:
;;   RAX
;;   RBX
;;   RCX
;;   R8
;;   R9
map_vals:
        ; check type
        mov al, BYTE [rsi]
        cmp al, maltype_empty_map
        je .empty_map

        and al, container_mask
        cmp al, container_map
        jne .empty_map          ; error
        
        xor r8, r8              ; Return list
        
.loop:
        ; Here should have a key in RSI

        ; First get the value
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done              ; error. Should be a value
        
        mov rsi, [rsi + Cons.cdr] ; Now have value in RSI
        
        ; Create a new Cons for this value
        call alloc_cons
        mov cl, BYTE [rsi]
        and cl, content_mask
        add cl, block_cons + container_list
        mov [rax], BYTE cl      ; Set type
        mov rbx, [rsi + Cons.car]
        mov [rax + Cons.car], rbx          ; Set value

        and cl, content_mask
        cmp cl, content_pointer
        jne .append

        ; A pointer, so increment reference count
        mov cx, WORD [rbx + Cons.refcount]
        inc cx
        mov [rbx + Cons.refcount], WORD cx
        
.append:
        cmp r8, 0
        je .first

        ; appending
        mov [r9 + Cons.typecdr], BYTE content_pointer
        mov [r9 + Cons.cdr], rax
        mov r9, rax
        jmp .next
.first:
        ; First key, so put into r8
        mov r8, rax
        mov r9, rax
.next:
        ; Get the next key
        mov al, BYTE [rsi + Cons.typecdr]
        cmp al, content_pointer
        jne .done
        mov rsi, [rsi + Cons.cdr]
        jmp .loop
.done:
        ; Finished, return the list
        mov rax, r8
        ret
        
.empty_map:
        ; return empty list
        call alloc_cons
        mov [rax], BYTE maltype_empty_list
        ret

        
;; ------------------------------------------------------------
;; Function type
;;
;; Functions are consist of a list
;;   - First car is the function address to call
;;   - Second is the Meta data (nil by default)
;;   - Third is the environment
;;   - Fourth is the binds list
;;   - Fifth is the body of the function
;;
;;   ( addr meta env binds body )
;;
;;

;; Address of native function in RSI
;; returns Function object in RAX
native_function:
        call alloc_cons         ; for meta
        mov [rax], BYTE maltype_nil
        push rax
        
        call alloc_cons         ; For function address
        mov [rax], BYTE (block_cons + container_function + content_function)
        mov [rax + Cons.car], rsi

        mov [rax + Cons.typecdr], BYTE content_pointer
        pop rbx                 ; meta
        mov [rax + Cons.cdr], rbx
        ret
