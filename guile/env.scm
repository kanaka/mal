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
  (export make-Env)
  (import (guile) (types)))

(define* (make-Env #:key (outer nil) (binds '()) (exprs '()))
  (define _env (make-hash-table))
  (define (_set k v) (hash-set! _env k v))
  (define (_get k)
    (or (hash-ref _env k) (and (not (_nil? outer)) ((outer 'find) k))))
  (define (_find k) (_get k))
  (for-each (lambda (b e) (hash-set! _env b e)) binds exprs)
  (lambda (cmd)
    (case cmd
      ((set) _set)
      ((find) _find)
      ((get) _get)
      (else (throw 'mal-error "BUG: Invalid cmd" cmd)))))
