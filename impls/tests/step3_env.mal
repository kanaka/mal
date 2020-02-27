;; Testing REPL_ENV
(+ 1 2)
;=>3
(/ (- (+ 5 (* 2 3)) 3) 4)
;=>2


;; Testing def!
(def! x 3)
;=>3
x
;=>3
(def! x 4)
;=>4
x
;=>4
(def! y (+ 1 7))
;=>8
y
;=>8

;; Verifying symbols are case-sensitive
(def! mynum 111)
;=>111
(def! MYNUM 222)
;=>222
mynum
;=>111
MYNUM
;=>222

;; Check env lookup non-fatal error
(abc 1 2 3)
;/.*\'?abc\'? not found.*
;; Check that error aborts def!
(def! w 123)
(def! w (abc))
w
;=>123

;; Testing let*
(let* (z 9) z)
;=>9
(let* (x 9) x)
;=>9
x
;=>4
(let* (z (+ 2 3)) (+ 1 z))
;=>6
(let* (p (+ 2 3) q (+ 2 p)) (+ p q))
;=>12
(def! y (let* (z 7) z))
y
;=>7

;; Testing outer environment
(def! a 4)
;=>4
(let* (q 9) q)
;=>9
(let* (q 9) a)
;=>4
(let* (z 2) (let* (q 9) a))
;=>4

;>>> deferrable=True
;;
;; -------- Deferrable Functionality --------

;; Testing let* with vector bindings
(let* [z 9] z)
;=>9
(let* [p (+ 2 3) q (+ 2 p)] (+ p q))
;=>12

;; Testing vector evaluation
(let* (a 5 b 6) [3 4 a [b 7] 8])
;=>[3 4 5 [6 7] 8]
