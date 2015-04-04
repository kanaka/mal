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

;;(use-modules (ice-9 readline))

(library (readline)
  (export _readline)
  (import (guile) (ice-9 readline)))

(define mal-history
  (format #f "~a/.mal-history" (getenv "HOME")))

(setenv "GUILE_HISTORY" mal-history)
(readline-set! bounce-parens 0)
(activate-readline)

(define (_readline prompt)
  (let ((str (readline prompt)))
    (and (not (eof-object? str)) (add-history str))
    str))
