#lang rosette
(require "./zhash.rkt")

; basic usage: concrete
(clear-vc!)
; - this creates a zhash with capacity 10 and default value zvoid
; - zvoid is a pre-defined symbol for representing empty value slot
;   you can use other values instead for better performance, e.g., #f
; - capacity 10 means the zhash can hold up to 10 different keys
(define z0 (make-zhash 10 zvoid))
(zhash-set! z0 "apple" 2)
(zhash-set! z0 20908 8)

; basic usage: symbolic key
(clear-vc!)
(define z1 (make-zhash 10 zvoid))
(define-symbolic b1 integer?)
(if (> b1 0) (zhash-set! z1 "apple" 2) (zhash-set! z1 "banana" 3))

; basic usage: capacity exception
; (clear-vc!)
; (define z2 (make-zhash 10 zvoid))
; (for ([i (range 10)]) (zhash-set! z2 i i))
; (zhash-set! z2 10 10)

; basic usage: symbolic key (union), concret value
(clear-vc!)
(define z3 (make-zhash 10 zvoid))
(define-symbolic b3 integer?)
(define l3 (list "apple" "banana" "cat" "dog"))
(define k3 (list-ref l3 b3))
(zhash-set! z3 k3 299)
(printf "3-0: ~v\n" (zhash-ref z3 "banana"))
(printf "3-1: ~v\n" (zhash-has-key? z3 "banana"))
(printf "3-2: ~v\n" (solve (assert (zhash-has-key? z3 "banana")))) ; expected: b3=1
(printf "3-3: ~v\n" (solve (assert (and (not (equal? b3 1)) (zhash-has-key? z3 "banana"))))) ; expected: unsat

; basic usage: symbolic key (union), symbolic value
(clear-vc!)
(define z4 (make-zhash 10 zvoid))
(define-symbolic b4 integer?)
(define l4 (list "apple" "banana" "cat" "dog"))
(define m4 (list 999 888 777 666))
(define k4 (list-ref l4 b4))
(define v4 (list-ref m4 b4))
(zhash-set! z4 k4 v4)
(printf "4-0: ~v\n" (solve (assert (equal? 777 (zhash-ref z4 "banana"))))) ; expected: unsat
(printf "4-1: ~v\n" (solve (assert (equal? 888 (zhash-ref z4 "banana"))))) ; expected: b4=1

; basic usage: symbolic execution
(clear-vc!)
(define z5 (make-zhash 10 zvoid))
(define-symbolic b5 integer?)
(if (> b5 9)
	(zhash-set! z5 "wonderful" 89)
	(zhash-set! z5 "better" 288)
)

; basic usage: symbolic execution + update
(clear-vc!)
(define z6 (make-zhash 10 zvoid))
(define-symbolic b6 integer?)
(if (> b6 29)
	(zhash-set! z6 "uurr" 87)
	(zhash-set! z6 "jjkk" 99)
)
(if (< b6 77)
	(zhash-set! z6 "uurr" 101)
	(zhash-set! z6 "jjkk" 202)
)
(printf "6-0: ~v\n" (solve (assert (equal? 101 (zhash-ref z6 "uurr"))))) ; expected: 29<b6<77
; jjkk --- 29 --- uurr
; uurr --- 77 --- jjkk
(printf "6-1: ~v\n" (solve (assert (not (zhash-has-key? z6 "uurr"))))) ; expected: unsat, because b needs to be: b<=29 and b>=77
(printf "6-2: ~v\n" (solve (assert (zhash-has-key? z6 "uurr")))) ; expected: 29<b<77
