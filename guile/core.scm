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
  (export core.ns ->list)
  (import (guile) (rnrs) (types) (reader) (printer) (ice-9 match)))

(define (->list o) ((if (vector? o) vector->list identity) o))

(define (_count obj)
  (cond
   ((_nil? obj) 0)
   ((vector? obj) (vector-length obj))
   (else (length obj))))

(define (_empty? obj) (zero? (_count obj)))

;; Well, strange spec...
(define (_equal? o1 o2)
  (equal? (->list o1) (->list o2)))

(define (pr-str . args)
  (define (pr x) (pr_str x #t))
  (string-join (map pr args) " "))

(define (str . args)
  (define (pr x) (pr_str x #f))
  (string-join (map pr args) ""))

(define (prn . args)
  (format #t "~a~%" (apply pr-str args))
  nil)  

(define (println . args)
  (define (pr x) (pr_str x #f))
  (format #t "~{~a~^ ~}~%" (map pr args) " ")
  nil)

(define (slurp filename)
  (when (not (file-exists? filename))
    (throw 'mal-error "File/dir doesn't exist" filename))
  (call-with-input-file filename get-string-all))

(define (_cons x y)
  (cons x (->list y)))

(define (concat . args)
  (apply append (map ->list args)))

(define *primitives*
  `((list        ,list)
    (list?       ,list?)
    (empty?      ,_empty?)
    (count       ,_count)
    (=           ,_equal?)
    (<           ,<)
    (<=          ,<=)
    (>           ,>)
    (>=          ,>=)
    (+           ,+)
    (-           ,-)
    (*           ,*)
    (/           ,/)
    (not         ,not)
    (pr-str      ,pr-str)
    (str         ,str)
    (prn         ,prn)
    (println     ,println)
    (read-string ,read_str)
    (slurp       ,slurp)
    (cons        ,_cons)
    (concat      ,concat)

))

;; Well, we have to rename it to this strange name...
(define core.ns *primitives*)
