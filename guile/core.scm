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

(library (core)
  (export core.ns)
  (import (guile) (types) (printer)))

(define (_count obj)
  (cond
   ((_nil? obj) 0)
   ((vector? obj) (vector-length obj))
   (else (length obj))))

(define (_empty? obj) (zero? (_count obj)))

(define (pr-str . args)
  (define (pr x) (pr_str x #t))
  (string-join (map pr args) " "))

(define *primitives*
  `((list    ,list)
    (list?   ,list?)
    (empty?  ,_empty?)
    (count   ,_count)
    (=       ,equal?)
    (<       ,<)
    (<=      ,<=)
    (>       ,>)
    (>=      ,>=)
    (+       ,+)
    (-       ,-)
    (*       ,*)
    (/       ,/)
    (not     ,not)
    (pr-str  ,pr-str)

))

;; Well, we have to rename it to this strange name...
(define core.ns *primitives*)
