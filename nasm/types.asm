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
;;  0    0 - Value (single boxed value for Cons blocks, vector for Array blocks).
;;  2    1 - List (value followed by pointer). Only for Cons blocks
;;  4    2 - Symbol (special char array). Only for Array blocks
;;  6    3 - Keyword
;;  8    4 - Map
;; 10    5 - Function
;;
;;  Content type [4 bits]:
;;   0   0 - Nil
;;  16   1 - Bool
;;  32   2 - Char
;;  48   3 - Int
;;  64   4 - Float
;;  80   5 - Pointer (memory address)
;;  96   6 - Function (instruction address)
;; 112   7 - Empty (distinct from Nil)
;;
;; These represent MAL data types as follows:
;;
;; MAL type     Block     Container     Content
;; --------- | -------- | ---------- | ---------
;; integer      Cons       Value         Int    
;; symbol       Array      Symbol        Char
;; list         Cons       List          Any
;; nil          Cons       Value         Nil
;; true         Cons       Value         Bool  (1)
;; false        Cons       Value         Bool  (0)
;; string       Array      Value         Char
;; keyword      Array      Keyword       Char
;; vector       Array      Value         Int/Float
;; hash-map     Cons       Map           Alternate key, values
;; atom         Cons       Value         Pointer
;;
        
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
%define block_cons  0
%define block_array 1

;; Container types
%define container_value  0
%define container_list 2
%define container_symbol 4
%define container_keyword 6
%define container_map 8
%define container_function 10

;; Content type
%define content_nil  0
%define content_bool 16
%define content_char 32
%define content_int 48
%define content_float 64
%define content_pointer 80      ; Memory pointer (to Cons or Array)
%define content_function 96     ; Function pointer
%define content_empty 112
        
;; Common combinations for MAL types
%define maltype_integer  (block_cons + container_value + content_int)
%define maltype_string  (block_array + container_value + content_char)
%define maltype_symbol  (block_array + container_symbol + content_char)
%define maltype_nil  (block_cons + container_value + content_nil)
%define maltype_empty_list (block_cons + container_list + content_empty)
%define maltype_empty_map (block_cons + container_map + content_empty)
        
;; ------------------------------------------

section .data
        
;; Fixed strings for printing

              
error_msg_print_string: db "Error in print string",10
.len: equ $ - error_msg_print_string
        
error_array_memory_limit: db "Error: Run out of memory for Array objects. Increase heap_array_limit.",10
.len: equ $ - error_array_memory_limit

error_cons_memory_limit: db "Error: Run out of memory for Cons objects. Increase heap_cons_limit.",10
.len: equ $ - error_cons_memory_limit
        
;; ------------------------------------------
;; Memory management
;;
;; For each object (Cons or Array), there is a block of memory (in BSS).
;; When an object is requested it is first taken from the free list
;; If the free list is empty (address 0) then the next object in the block
;; is used, and the heap_x_number counter is incremented. When an object
;; is free'd it is pushed onto the heap_x_free list.
        
        
%define heap_cons_limit 10     ; Number of cons objects which can be created

heap_cons_next: dd  heap_cons_store  ; Address of next cons in memory
heap_cons_free: dq 0            ; Address of start of free list
        
%define heap_array_limit 10     ; Number of array objects which can be created
        
heap_array_next: dd heap_array_store
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
release_cons:
        mov ax, WORD [rsi + Cons.refcount]
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


;; Releases either a Cons or Array
;; Address of object in RSI
release_object:
        mov al, BYTE [rsi]          ; Get first byte
        and al, block_mask          ; Test block type
        cmp al, block_array         ; Test if it's an array
        je .array
        call release_cons
        ret
.array:
        call release_array
        ret

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
;; String type

;; Create a new string, address in RAX
string_new:
        call alloc_array
        mov [rax], BYTE maltype_string
        mov QWORD [rax + Array.next], 0
        ret

;; Convert a raw string to a String type
;;
;; Input: Address of raw string in RSI, length in EDX
;; Output: Address of string in RAX
;;
;; Modifies registers: R8,R9,RCX
raw_to_string:
        push rsi
        push rdx
        call string_new         ; String now in RAX
        pop rdx
        pop rsi
        mov [rax + Array.length], DWORD edx
        mov r8, rax
        add r8, Array.data      ; Address of string data
        mov r9, rsi             ; Address of raw data
        mov ecx, edx            ; Count
.copy_loop:
        
        mov bl, BYTE [r9]
        mov [r8], BYTE bl
        inc r8
        inc r9
        dec ecx
        jnz .copy_loop
        ret
        
        
        
;; Appends a character to a string
;; Input: Address of string in RSI, character in CL
string_append_char:
        mov eax, DWORD [rsi + Array.length]
        inc eax
        mov DWORD [rsi + Array.length], eax
        dec eax
        add rax, rsi
        add rax, Array.data            ; End of data
        mov [rax], BYTE cl        
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
        
        ; Find the end of the string in RSI
        ; and put the address of the Array object into rax
        mov rax, rsi
.find_string_end:
        mov r8, QWORD [rax + Array.next]
        cmp r8, 0               ; Next chunk is null
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
        cmp rbx, 0              ; Test if it's null
        je .finished            ; No more, so we're done
        ; Move on to next Array object
        
        ; Get source address into r10
        mov r10, rbx
        add r10, Array.data     ; Start of the data

        ; Source end address
        mov r11, rbx
        add r11, Array.size
        
.source_ok:

        ; Move destination to next byte
        inc r8
        ; Check if we've reached end of the Array
        cmp r8, r9
        jne .copy_loop          ; Next byte

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
        
.finished:
        ; Compare r8 (destination) with data start
        ; to get length of string
        sub r8, rax
        sub r8, Array.data
        inc r8
        ; r8 now contains length
        mov DWORD [rax + Array.length], r8d
        
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
;;    RCX
;;    RBX
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

        ; Don't care about container type
        and cl, block_mask + content_mask
        and bl, block_mask + content_mask
        
        cmp bl, cl              ; compare block and content
        jne .different_types

        ; Here the same block, content type
        ; May be different container (value/list, string/symbol)
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

        ; equal
        inc rbx
        inc rcx
        dec eax
        jnz .compare_loop

        ; equal
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
        
map_new:
        call alloc_cons
        mov [rax], BYTE (block_cons + container_map + content_empty)
        mov [rax + Cons.typecdr], BYTE content_nil
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
;; Inputs: RSI - map
;;         RDI - key
;;
;; Outputs: RAX - Cons object containing value in CAR
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
        
        ; increment reference count
        mov ax, WORD [rsi + Cons.refcount]
        inc ax
        mov [rsi + Cons.refcount], WORD ax
        ; Put address in rax
        mov rax, rsi
        ret

.not_found:
        lahf                    ; flags in AH
        and ah, 255-64          ; remove zero flag
        sahf
        
        ; last cons in rsi
        ; increment reference count
        mov ax, WORD [rsi + Cons.refcount]
        inc ax
        mov [rsi + Cons.refcount], WORD ax
        ; Put address in rax
        mov rax, rsi
        
        ret

.error:
        
        lahf                    ; flags in AH
        or ah, 255-64               ; set zero flag
        sahf
        
        ; return nil
        call alloc_cons
        mov [rax], BYTE maltype_nil
        mov [rax + Cons.typecdr], BYTE content_nil
        ret
        
