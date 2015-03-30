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

(library (types)
  (export string-sub *eof*
          _keyword _keyword?
          nil _nil?
          cond-true?
          atom atom? atom-val atom-val-set!)
  (import (guile) (rnrs) (ice-9 regex)))

(define (string-sub str p1 p2)
  (regexp-substitute/global #f p1 str 'pre p2 'post))

(define *eof* (call-with-input-string "" read))

(define (_keyword? k)
  (and (string? k) (string-match "^\u029e")))

(define (_keyword str)
  (string-append "\u029e" str))

(define nil 'nil)

(define (_nil? obj) (eq? nil obj))

(define (cond-true? obj)
  (and (not (_nil? obj)) obj))

(define-record-type atom (fields (mutable val)))
