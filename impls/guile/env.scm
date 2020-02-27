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

(library (env)
  (export make-Env env-has env-check)
  (import (guile) (types)))

(define (env-check sym env)
  (env-has sym env (lambda _ #f)))

(define (sym-err-throw sym)
  (throw 'mal-error (format #f "'~a' not found" sym)))

(define* (env-has sym env #:optional (err sym-err-throw))
  (let ((v ((env 'get) sym)))
    (if (equal? v '*mal-null*)
        (err sym)
        v)))

(define* (make-Env #:key (outer nil) (binds '()) (exprs '()))
  (define _env (make-hash-table))
  (define (_set k v) (hash-set! _env k v))
  (define (_get k)
    (let ((v (hash-ref _env k '*mal-null*)))
      (if (equal? v '*mal-null*)
          (if (_nil? outer)
              '*mal-null*
              ((outer 'get) k))
          v)))
  (define (_find k) (_get k))
  (define (_show)
    (hash-for-each (lambda (k v) (format #t "~a : ~a~%" k v)) _env)
    (display "outer:\n")
    (and (not (_nil? outer)) ((outer 'show))))
  (let lp((b binds) (e exprs))
    (cond
     ((null? b) #t)
     ((eq? (car b) '&) (hash-set! _env (cadr b) e)) ; handle varglist
     (else ; normal binding
      (when (not (symbol? (car b)))
        (throw 'mal-error (format #f "Invalid binding key! '~a'" (car b))))
      (when (null? e)
        (throw 'mal-error "Invalid pattern for this macro"))
      (hash-set! _env (car b) (car e))
      (lp (cdr b) (cdr e)))))
  (lambda (cmd)
    (case cmd
      ((set) _set)
      ((find) _find)
      ((get) _get)
      ((show) _show)
      (else (throw 'mal-error (format #f "BUG: Invalid cmd '~a'" cmd))))))
