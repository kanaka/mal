;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (printer)
         (export pr_str)
         (import (guile) (types) (ice-9 match) (ice-9 regex)))

(define (print-hashmap hm p)
  (call-with-output-string
   (lambda (port)
     (display "{" port)
     (display
      (string-join
       (hash-map->list
        (lambda (k v)
          (format #f "~a ~a" (p k) (p v)))
        hm)
       " ")
      port)
     (display "}" port))))

(define (pr_str obj readable?)
  (define (->str s)
    (string-sub
     (string-sub
      (string-sub s "\\\\" "\\\\")
      "\"" "\\\"")
     "\n" "\\n"))
  (define (%pr_str o) (pr_str o readable?))
  (match obj
    ((? box?) (%pr_str (unbox obj)))
    ((? is-func?) "#<function>")
    ((? is-macro?) "#<macro>")
    ((? list?) (format #f "(~{~a~^ ~})" (map %pr_str obj)))
    ((? vector?) (format #f "[~{~a~^ ~}]" (map %pr_str (vector->list obj))))
    ((? hash-table?) (print-hashmap obj %pr_str))
    ((? string?)
     (cond
      ((_keyword? obj)
       => (lambda (m) (format #f ":~a" (substring obj 1))))
      (else (if readable? (format #f "\"~a\"" (->str obj)) obj))))
    ;;((? number?) (format #f "~a" obj))
    ;;((? symbol?) (format #f "~a" obj))
    ((? atom?) (format #f "(atom ~a)" (%pr_str (atom-val obj))))
    ((? _nil?) "nil")
    (#t "true")
    (#f "false")
    (else (format #f "~a" obj))))
