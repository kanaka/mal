(de load-history (File)
   (when (info File)
      (in File
         (until (eof)
            (native "libreadline.so" "add_history" NIL (line T)) ) ) ) )

(de save-to-history (Input)
   (when Input
      (native "libreadline.so" "add_history" NIL Input)
      (out "+.mal_history"
         (prinl Input) ) ) )

(de readline (Prompt)
   (let Input (native "libreadline.so" "readline" 'N Prompt)
      (if (=0 Input)
         0
         (prog1
            (struct Input 'S)
            (save-to-history @) ) ) ) )
