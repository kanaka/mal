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
     (for-each
      (lambda (h)
        (format port "~a ~a" (car h) (cdr h)))
      (hash-map->list
       (lambda (k v)
         (cons (p k) (p v)))
       hm))
     (display "}" port))))

(define (pr_str obj readable?)
  (define (%pr_str o) (pr_str o readable?))
  (match obj
    ((? list?) (format #f "(~{~a~^ ~})" (map %pr_str obj)))
    ((? vector?) (format #f "[~{~a~^ ~}]" (map %pr_str (vector->list obj))))
    ((? hash-table?) (print-hashmap obj %pr_str))
    ((? string?)
     (cond
      ((string-match "^\u029e(.*)" obj)
       => (lambda (m) (format #f ":~a" (match:substring m 1))))
      (else (if readable? (format #f "\"~a\"" obj) obj))))
    ((? number?) obj)
    ((? symbol?) obj)
    ((? atom?) (format #f "(atom ~a)" (atom-val obj)))
    ((? _nil?) nil)
    (#t "true")
    (#f "false")
    (else (display obj))))
