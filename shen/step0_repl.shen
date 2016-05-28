\\ FIXME: Ruby interop to get readline support
\\ (lineread) d:es parsing of the input
(rb.#require "readline")

(define main
  [] -> (let line (rb.#Readline.readline "user> ")
          (if (string? line)
            (do 
              (output "~A~%" line)
              (main []))
            false)))

(main [])
