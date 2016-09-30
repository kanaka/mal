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
   (prog1
      (native "libreadline.so" "readline" 'S Prompt)
      (save-to-history @) ) )

