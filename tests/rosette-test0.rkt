#lang rosette

(define list0 (list 100 101 102 103 104 105))
(define-symbolic ptr0 integer?)
(define v0 (list-ref list0 ptr0))
(for/all ([v v0 #:exhaustive]) (printf "~a," (vc)) (printf "~a\n" v))
(printf "======\n")
(define v01 (for/all ([v v0 #:exhaustive]) (+ 1 v)))

(define list1 (list "apple" "banana" "cat" "dog"))
(define-symbolic ptr1 integer?)
(define v1 (list-ref list1 ptr1))
(for/all ([v v1 #:exhaustive]) (printf "~a," (vc)) (printf "~a\n" v))
(printf "======\n")

(define-symbolic ptr2 integer?)
(for/all ([p ptr2 #:exhaustive]) (printf "~a," (vc)) (printf "~a\n" p))
(printf "======\n")

; (define list3 (list 5 4 3 2 1 0))
; (define-symbolic ptr3 integer?)
; (define v3 (list-ref list0 (list-ref list3 ptr3)))
; (for/all ([p v3 #:exhaustive]) (printf "~a," (vc)) (printf "~a\n" p))
; (printf "======\n")