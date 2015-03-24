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
  (export *eof* _keyword _keyword?
          nil _nil?
          atom atom? atom-val atom-val-set!)
  (import (guile) (rnrs)))

(define *eof* (call-with-input-string "" read))

(define (_keyword? k)
  (and (string? k) (string-match "^\u029e")))

(define (_keyword str)
  (string-append "\u029e" str))

(define nil 'nil)

(define (_nil? obj) (eq? nil obj))

(define-record-type atom (fields (mutable val)))
