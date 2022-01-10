#lang rosette
(define list0 (list))
(define-symbolic ind integer?)
(if (> ind 0) (set! list0 (cons 9 list0)) (set! list0 (cons 21 (cons 20 list0))))

(define list1 (list))
(define-symbolic ink integer?)
(if (> ink 0) (set! list1 (cons 8 (cons 9 list1))) (set! list1 (cons 21 (cons 20 list1))))

(define list2 (list))
(define-symbolic inj integer?)
(if (> inj 0)
    (with-vc vc-true (set! list2 (cons 8 (cons 9 list2))))
    (with-vc vc-true (set! list2 (cons 21 (cons 20 list2))))
)