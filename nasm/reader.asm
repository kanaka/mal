
section .bss

;; State of Reader
        
        
section .text

read_str:
        ; Convert the input string into a list of tokens
        call tokenizer
        ; RAX now contains address of list of tokens
        ret
        
;; -----------------------------
;; list(tokens) tokenizer (string)
;;
;; Input string address in RSI
;; Creates a list of tokens, returns address in RAX
tokenizer:

        ; Get a new 
        call alloc_array
        ret
